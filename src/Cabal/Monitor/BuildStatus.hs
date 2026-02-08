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
    parseStatusInfix,

    -- * Elimination
    FormatStyle (..),
    formatStatusInit,
    formatStatusFinal,

    -- * Functions
    numAllPkgs,
  )
where

import Cabal.Monitor.Args (Coloring (unColoring))
import Cabal.Monitor.Pretty qualified as Pretty
import Control.Applicative (asum)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable qualified as F
import Data.Kind (Type)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
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
    -- | Packages that have completed building. Same for both phases.
    completed :: Set Package
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup (BuildStatus p) where
  MkBuildStatus x1 x2 x3 <> MkBuildStatus y1 y2 y3 =
    MkBuildStatus (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance Monoid (BuildStatus p) where
  mempty = MkBuildStatus mempty mempty mempty

type BuildStatusInit = BuildStatus BuildStatusPhaseInit

type BuildStatusFinal = BuildStatus BuildStatusPhaseFinal

-- | Parses a status.
parseStatus :: ByteString -> BuildStatusInit
parseStatus = parseStatus' searches
  where
    searches bs =
      [ mkInit BS.stripPrefix mkPkg (BS.takeWhile (/= spaceChr)) " - " bs,
        mkInit BS.stripPrefix mkBuilding takeSkipLeadingSpc "Starting" bs,
        mkInit BS.stripPrefix mkCompleted takeSkipLeadingSpc "Completed" bs
      ]

-- | Like 'parseStatus', except does an additional pass that searches for
-- infix patterns, if the prefix patterns are not found. This is more
-- flexible but significantly slower.
parseStatusInfix :: ByteString -> BuildStatusInit
parseStatusInfix = parseStatus' searches
  where
    searches bs =
      [ mkInit BS.stripPrefix mkPkg (BS.takeWhile (/= spaceChr)) " - " bs,
        mkInit BS.stripPrefix mkBuilding takeSkipLeadingSpc "Starting" bs,
        mkInit BS.stripPrefix mkCompleted takeSkipLeadingSpc "Completed" bs,
        mkInit stripInfix' mkPkg (BS.takeWhile (/= spaceChr)) " - " bs,
        mkInit stripInfix' mkBuilding takeSkipLeadingSpc "Starting" bs,
        mkInit stripInfix' mkCompleted takeSkipLeadingSpc "Completed" bs
      ]

    stripInfix' bs1 = fmap snd . stripInfix bs1

parseStatus' :: (ByteString -> [Maybe BuildStatusInit]) -> ByteString -> BuildStatusInit
parseStatus' searches = F.foldMap' go . C8.lines
  where
    go txt = fromMaybe mempty $ asum (searches txt)

mkInit ::
  -- Search function
  (ByteString -> ByteString -> Maybe ByteString) ->
  -- BuildStatusInit constructor.
  (ByteString -> BuildStatusInit) ->
  -- Bytestring parse fn.
  (ByteString -> ByteString) ->
  -- Bytestring prefix to match.
  ByteString ->
  ByteString ->
  Maybe BuildStatusInit
mkInit searchFn cons parseFn pfx txt = cons . parseFn <$> searchFn pfx txt

spaceChr :: Word8
spaceChr = 32

takeSkipLeadingSpc :: ByteString -> ByteString
takeSkipLeadingSpc = BS.takeWhile (/= spaceChr) . BS.dropWhile (== spaceChr)

mkPkg :: ByteString -> BuildStatusInit
mkPkg lib =
  MkBuildStatus
    { toBuild = Set.singleton $ MkPackage lib,
      building = mempty,
      completed = mempty
    }

mkBuilding :: ByteString -> BuildStatusInit
mkBuilding lib =
  MkBuildStatus
    { toBuild = mempty,
      building = Set.singleton $ MkPackage lib,
      completed = mempty
    }

mkCompleted :: ByteString -> BuildStatusInit
mkCompleted lib =
  MkBuildStatus
    { toBuild = mempty,
      building = mempty,
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
      completed = status.completed
    }
  where
    toBuild =
      status.toBuild
        \\ (status.building `Set.union` status.completed)

    building = status.building \\ status.completed

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

takeCount :: Natural -> [a] -> (Natural, [a])
takeCount k = fmap L.reverse . go (k, [])
  where
    go acc [] = acc
    go acc@(0, _) _ = acc
    go (!cnt, zs) (x : xs) = go (cnt - 1, x : zs) xs

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
numAllPkgs :: BuildStatusInit -> Int
numAllPkgs status = Set.size status.toBuild

-- | @stripInfix needle haystack@ returns @Just (pre, post)@ iff
-- @haystack === pre needle post@. Otherwise returns @Nothing@.
stripInfix :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
stripInfix bs1 bs2 = (pre,) <$> BS.stripPrefix bs1 rest
  where
    (pre, rest) = BS.breakSubstring bs1 bs2
