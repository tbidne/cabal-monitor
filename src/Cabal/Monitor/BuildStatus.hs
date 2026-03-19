module Cabal.Monitor.BuildStatus
  ( -- * Types
    BuildStatus (..),
    Package (..),

    -- ** Phases
    BuildStatusInit,
    BuildStatusFinal,
    BuildStatusPhase (..),
    advancePhase,

    -- * Construction
    parseStatus,

    -- * Elimination
    FormatStyle (..),
    formatStatusInit,
    formatStatusFinal,

    -- * Functions
    numAllPkgs,
  )
where

import Cabal.Monitor.Args
  ( Coloring (unColoring),
    LocalPackages (unLocalPackages),
    SearchInfix (unSearchInfix),
  )
import Cabal.Monitor.Pretty qualified as Pretty
import Control.Applicative (asum)
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Kind (Type)
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word8)
import FileSystem.UTF8 qualified as UTF8
import GHC.Generics (Generic)
import GHC.Natural (Natural)

-- | Package.
newtype Package = MkPackage {unPackage :: ByteString}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (IsString)
  deriving anyclass (NFData)

-- | Describes the possible 'phasess' for the status.
data BuildStatusPhase
  = -- | Phase right after the file is read.
    BuildStatusPhaseInit
  | -- | Phase after the file is processed.
    BuildStatusPhaseFinal
  deriving stock (Eq, Show)

-- | Build status.
type BuildStatus :: BuildStatusPhase -> Type
data BuildStatus p = MkBuildStatus
  { -- | During the 'BuildStatusPhaseInit' phase, this includes all packages
    -- that will be built. In the 'BuildStatusPhaseFinal' phase, this is only
    -- the packages that have yet to be built.
    toBuild :: Set Package,
    -- | During the 'BuildStatusPhaseInit' phase, this includes all packages
    -- that have started building i.e. will include those whose builds have
    -- also completed. In the 'BuildStatusPhaseFinal' phase, this is only the
    -- packages currently building.
    building :: Set Package,
    -- | Local packages that are being built. These require special care for
    -- detecting completion.
    --
    -- During the 'BuildStatusPhaseInit' phase, this is like 'building' i.e.
    -- all local packages that have started building, including completions.
    --
    -- In the 'BuildStatusPhaseFinal' phase, this is empty, as we do not need
    -- the local distinction when rendering the status.
    buildingLocal :: Set Package,
    -- | Packages that have completed building. Same for both phases.
    completed :: Set Package
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup (BuildStatus p) where
  MkBuildStatus x1 x2 x3 x4 <> MkBuildStatus y1 y2 y3 y4 =
    MkBuildStatus (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4)

instance Monoid (BuildStatus p) where
  mempty = MkBuildStatus mempty mempty mempty mempty

type BuildStatusInit = BuildStatus BuildStatusPhaseInit

type BuildStatusFinal = BuildStatus BuildStatusPhaseFinal

-- | Parses a status.
parseStatus ::
  -- | Whether to apply special local packages handling.
  LocalPackages ->
  -- | Whether to search for infix patterns.
  SearchInfix ->
  -- | ByteString to parse.
  ByteString ->
  BuildStatusInit
parseStatus localPackages searchInfix = go mempty . C8.lines
  where
    stripFn =
      if searchInfix.unSearchInfix
        then stripInfix'
        else BS.stripPrefix

    go acc [] = acc
    go acc (l : ls) = case parseContextFreeLine acc l of
      Just acc' -> go (acc' <> acc) ls
      Nothing ->
        if isInit l
          then
            let (acc', rest) = parseInit mempty ls
             in go (acc' <> acc) rest
          else go acc ls

    isInit = isJust . stripFn "In order, the following will be built"

    parseInit acc [] = (acc, [])
    parseInit acc (l : ls) = case stripFn " - " l of
      Nothing -> (acc, l : ls)
      Just r1 ->
        let acc' = acc <> mkPkg (BS.takeWhile (/= spaceChr) r1)
         in parseInit acc' ls

    parseContextFreeLine acc bs = asum $ ($ bs) <$> contextFreeLineParsers acc

    contextFreeLineParsers acc =
      if localPackages.unLocalPackages
        then coreParsers ++ localPackageParsers acc
        else coreParsers

    coreParsers =
      [ parseStarting,
        parseCompleted
      ]

    localPackageParsers acc =
      [ parseLocal,
        parseLocalCompleted acc
      ]

    parseStarting bs = do
      r1 <- stripFn "Starting " bs
      let r2 = takeSkipLeadingSpc r1
      pure $ mkBuilding r2

    parseCompleted bs = do
      r1 <- stripFn "Completed " bs
      let r2 = takeSkipLeadingSpc r1
      pure $ mkCompleted r2

    parseLocal bs = do
      r1 <- stripFn "Building " bs
      r2 <-
        asum
          [ BS.stripPrefix "library for " r1,
            parseLocalNamed "test suite '" r1,
            parseLocalNamed "executable '" r1
          ]
      mkBuildingLocal <$> takeUntilEq "..." r2

    parseLocalNamed name bs = do
      r1 <- BS.stripPrefix name bs
      let r2 = BS.dropWhile (/= squoteW8) r1
      BS.stripPrefix "' for " r2

    -- NOTE: [Local packages]
    --
    -- Local packages have a different completion check. Rather than the
    -- simple "Completed <pkg" log, they instead don't have anything.
    -- The final log that mentions the package name looks like (one line):
    --
    --   [148 of 148] Compiling Distribution.Simple
    --     ( src/Distribution/Simple.hs,
    --       /home/cabal/dist-newstyle/build/x86_64-linux/ghc-9.12.2/
    --         Cabal-3.17.0.0/build/Distribution/Simple.o,
    --       /home/cabal/dist-newstyle/build/x86_64-linux/ghc-9.12.2/
    --         Cabal-3.17.0.0/build/Distribution/Simple.dyn_o
    --     )
    --
    -- So we can infer that the package has completed successfully when we
    -- detect [M of N] and M == N. To find the package name, we scan all
    -- known "local packages" until we have an infix match in the rest of the
    -- string.
    --
    -- The complexity here is pretty terrible! For each line that matches
    -- [N of N], we have O(n) for number of local packages, then O(k + p)
    -- where k is the length of the bytestring and p is the length of the
    -- package name.
    --
    -- This is pretty bad, but we don't have another way that also works
    -- with --semaphore (logs can be out of order).
    parseLocalCompleted acc bs = do
      -- Minor optimization, don't even try parsing completed if we do not
      -- have any local packages.
      if Set.null acc.buildingLocal
        then Nothing
        else do
          r1 <- parseModuleLog bs

          -- if any local packages match this, then that should be completed.
          mkCompleted <$> asum (Set.map (pInPath r1) acc.buildingLocal)
      where
        pInPath r p =
          let p' = p.unPackage
           in if p' `BS.isInfixOf` r
                then Just p'
                else Nothing

    parseModuleLog bs = do
      (pre, post) <- breaksEqStrip " of " bs
      let d1 = BS.takeWhileEnd isDigit pre
      let d2 = BS.takeWhile isDigit post
      guard (d1 == d2)
      pure post

-- | 'stripInfix' that throws away the prefix.
--
-- >>> stripInfix' "foo" "barfoobaz"
-- Just "baz"
--
-- >>> stripInfix' "foo" "barfobaz"
-- Nothing
stripInfix' :: ByteString -> ByteString -> Maybe ByteString
stripInfix' bs1 = fmap snd . stripInfix bs1

-- | Like @take notEq@, except operates on a bytestring, rather than single word8.
--
-- >>> takeUntilEq "foo" "barfoobaz"
-- Just "bar"
--
-- >>> takeUntilEq "foo" "barfobaz"
-- Nothing
takeUntilEq :: ByteString -> ByteString -> Maybe ByteString
takeUntilEq n bs = BS.pack <$> go n' bs'
  where
    go :: [Word8] -> [Word8] -> Maybe [Word8]
    go [] rs = Just rs
    go (_ : _) [] = Nothing
    go wss@(w : ws) (r : rs) =
      if w == r && ws `L.isPrefixOf` rs
        then Just []
        else (r :) <$> go wss rs

    n' = BS.unpack n
    bs' = BS.unpack bs

-- | Like 'takeUntilEq', except returns both (non-inclusive) sides of the match.
--
-- >>> breaksEqStrip "foo" "barfoobaz"
-- Just ("bar","baz")
--
-- >>> breaksEqStrip "foo" "barfobaz"
-- Nothing
breaksEqStrip :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
breaksEqStrip n bs = bimap BS.pack BS.pack <$> go n' bs'
  where
    go :: [Word8] -> [Word8] -> Maybe ([Word8], [Word8])
    go [] rs = Just (rs, [])
    go (_ : _) [] = Nothing
    go wss@(_ : _) rss@(r : rs) =
      -- Using L.isPrefixOf instead of stripPrefix would instead give us
      -- breaksEq i.e. the needle would be part of the second value.
      case wss `L.stripPrefix` rss of
        Just rest -> Just ([], rest)
        Nothing -> first (r :) <$> go wss rs

    n' = BS.unpack n
    bs' = BS.unpack bs

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57

-- Single quote: '
squoteW8 :: Word8
squoteW8 = 39

spaceChr :: Word8
spaceChr = 32

-- | Skips leading space, takes all trailing non-space.
--
-- >>> takeSkipLeadingSpc "  foobar baz"
-- "foobar"
takeSkipLeadingSpc :: ByteString -> ByteString
takeSkipLeadingSpc = BS.takeWhile (/= spaceChr) . BS.dropWhile (== spaceChr)

mkPkg :: ByteString -> BuildStatusInit
mkPkg lib =
  MkBuildStatus
    { toBuild = Set.singleton $ MkPackage lib,
      building = mempty,
      buildingLocal = mempty,
      completed = mempty
    }

mkBuilding :: ByteString -> BuildStatusInit
mkBuilding lib =
  MkBuildStatus
    { toBuild = mempty,
      building = Set.singleton $ MkPackage lib,
      buildingLocal = mempty,
      completed = mempty
    }

mkBuildingLocal :: ByteString -> BuildStatusInit
mkBuildingLocal lib =
  MkBuildStatus
    { toBuild = mempty,
      building = mempty,
      buildingLocal = Set.singleton $ MkPackage lib,
      completed = mempty
    }

mkCompleted :: ByteString -> BuildStatusInit
mkCompleted lib =
  MkBuildStatus
    { toBuild = mempty,
      building = mempty,
      buildingLocal = mempty,
      completed = Set.singleton $ MkPackage lib
    }

data FormatStyle
  = -- | Format each package on a newline.
    FormatNl
  | -- | Format each package on a newline, truncating lines exceeding the
    -- given height.
    FormatNlTrunc Natural
  | -- | Format packages inline, creating a newline once we exceed the
    -- given width.
    FormatInl Natural
  | -- | Given height and width, formats inline (width) and truncates
    -- (height).
    FormatInlTrunc Natural Natural

-- | Advances the phase.
advancePhase :: BuildStatusInit -> BuildStatusFinal
advancePhase status =
  MkBuildStatus
    { toBuild,
      building,
      buildingLocal = mempty,
      completed = status.completed
    }
  where
    toBuild =
      status.toBuild
        \\ (building' `Set.union` status.completed)

    building' = status.building `Set.union` status.buildingLocal

    building = building' \\ status.completed

-- | Advances and formats the status.
formatStatusInit :: Coloring -> FormatStyle -> BuildStatusInit -> Text
formatStatusInit coloring style = formatStatusFinal coloring style . advancePhase

-- | Formats the status.
formatStatusFinal :: Coloring -> FormatStyle -> BuildStatusFinal -> Text
formatStatusFinal coloring style status =
  UTF8.decodeUtf8Lenient $
    BSL.toStrict $
      BSB.toLazyByteString $
        formatAll
          coloring
          style
          status.toBuild
          status.building
          status.completed

formatAll ::
  Coloring ->
  FormatStyle ->
  Set Package ->
  Set Package ->
  Set Package ->
  Builder
formatAll coloring style toBuildS buildingS completedS = final
  where
    final =
      mconcat $
        concatter
          toBuildBuilders
          buildingBuilders
          completedBuilders

    (formatter, concatter) = case style of
      FormatNl -> (formatNewlines, concatNewlines)
      FormatNlTrunc height -> (formatNewlines, concatTruncate height)
      FormatInl width -> (formatInline width, concatNewlines)
      FormatInlTrunc height width -> (formatInline width, concatTruncate height)

    concatTruncate height as bs cs =
      let -- Take all building that we can, get remaining height.
          (hRemaining, bs') = takeCount height bs

          -- Calculate heights. hEach is the max amount each remaining
          -- section gets if both are too large.
          hEach = hRemaining `div` 2
          numAs = fromIntegral $ length as
          numCs = fromIntegral $ length cs

          (as', cs') =
            if
              -- 1. num_as < hEach: Take all as, use the rest for cs.
              | numAs < hEach ->
                  let (hLeft, as_r) = takeCount hRemaining as
                   in (as_r, takeTrunc hLeft cs)
              -- 2. num_cs < hEach: Take all cs, use the rest for as.
              | numCs < hEach ->
                  let (hLeft, cs_r) = takeCount hRemaining cs
                   in (takeTrunc hLeft as, cs_r)
              -- 3. Neither fits completely. Divide evenly.
              | otherwise -> (takeTrunc hEach as, takeTrunc hEach cs)
       in concatNewlines as' bs' cs'

    -- NOTE: We do the coloring here since the "safe" way to color functions,
    -- i.e. the color function, operates on a single string type, not a list.
    concatNewlines as bs cs =
      if coloring.unColoring
        then
          mconcat
            [ [Pretty.magenta],
              as,
              [Pretty.endCode, "\n", "\n", Pretty.yellow],
              bs,
              [Pretty.endCode, "\n", "\n", Pretty.green],
              cs,
              [Pretty.endCode]
            ]
        else
          mconcat
            [ as,
              ["\n", "\n"],
              bs,
              ["\n", "\n"],
              cs
            ]

    toBuildBuilders = formatSet "To Build: " toBuildS
    buildingBuilders = formatSet "Building: " buildingS
    completedBuilders = formatSet "Completed: " completedS

    formatSet pfx s =
      let xs = Set.toList s
          bs = formatter xs
       in pfx <> BSB.intDec (Set.size s) : bs

formatNewlines :: [Package] -> [Builder]
formatNewlines = L.reverse . foldl' go []
  where
    go acc p = prependNewline (BSB.byteString p.unPackage) : acc

formatInline :: Natural -> [Package] -> [Builder]
formatInline width = L.reverse . fmap fst . foldl' go []
  where
    go :: [(Builder, Natural)] -> Package -> [(Builder, Natural)]
    go [] p =
      let (newBuilder, newLen) = pkgToData p
       in [(prependNewline newBuilder, newLen + newlineIdent)]
    go ((currBuilder, currLen) : accs) p =
      let (newBuilder, newLen) = pkgToData p
          totalLen = newLen + currLen + newPkgIdent
       in if totalLen + 1 > width
            then
              (prependNewline newBuilder, newLen + newlineIdent)
                : (currBuilder, currLen)
                : accs
            else
              (currBuilder <> ", " <> newBuilder, totalLen)
                : accs

    newlineIdent = 4
    newPkgIdent = 2

    pkgToData p =
      let bs = p.unPackage
       in (BSB.byteString bs, int2Nat $ BS.length bs)

prependNewline :: Builder -> Builder
prependNewline b = "\n  - " <> b

-- | @takeCount n xs@ takes up to @n@ elements from @xs@, and returns the
-- difference.
--
-- >>> takeCount 5 [1, 2]
-- (3,[1,2])
--
-- >>> takeCount 3 [1,2,3,4,5]
-- (0,[1,2,3])
takeCount :: Natural -> [a] -> (Natural, [a])
takeCount k = fmap L.reverse . go (k, [])
  where
    go acc [] = acc
    go acc@(0, _) _ = acc
    go (!cnt, zs) (x : xs) = go (cnt - 1, x : zs) xs

-- | @takeTrunc n xs@ takes up to @n@ elements from @xs@, appending an
-- ellipsis if it could not take everything.
--
-- >>> takeTrunc 5 ["1", "2"]
-- ["1","2"]
--
-- >>> takeTrunc 3 ["1", "2", "3", "4", "5"]
-- ["1","2","\n  ..."]
takeTrunc :: Natural -> [Builder] -> [Builder]
-- 1. Cannot take anymore.
takeTrunc 0 acc = acc
-- 2. No more to take.
takeTrunc _ [] = []
-- 3. Can take 1 but more than one left: add ellipsis.
takeTrunc 1 (_ : _ : _) = ["\n  ..."]
-- 4. General case: take 1, recurse.
takeTrunc !n (b : bs) = b : takeTrunc (n - 1) bs

int2Nat :: Int -> Natural
int2Nat = fromIntegral

-- | Returns the size of all packages we want to build.
numAllPkgs :: BuildStatusFinal -> Int
numAllPkgs status =
  Set.size $
    Set.unions
      [ status.toBuild,
        status.building,
        status.buildingLocal,
        status.completed
      ]

-- | @stripInfix needle haystack@ returns @Just (pre, post)@ iff
-- @haystack === pre needle post@. Otherwise returns @Nothing@.
--
-- >>> stripInfix "foo" "barfoobaz"
-- Just ("bar","baz")
--
-- >>> stripInfix "foo" "barfobaz"
-- Nothing
stripInfix :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
stripInfix bs1 bs2 = (pre,) <$> BS.stripPrefix bs1 rest
  where
    (pre, rest) = BS.breakSubstring bs1 bs2
