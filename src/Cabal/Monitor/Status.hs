module Cabal.Monitor.Status
  ( -- * Types
    Status (..),
    Package (..),

    -- * Construction
    parseStatus,

    -- * Elimination
    FormatStyle (..),
    formatStatus,
  )
where

import Cabal.Monitor.Pretty qualified as Pretty
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable qualified as F
import Data.List qualified as L
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import FileSystem.UTF8 qualified as UTF8
import GHC.Generics (Generic)
import GHC.Natural (Natural)

-- | Package.
newtype Package = MkPackage {unPackage :: ByteString}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (IsString)
  deriving anyclass (NFData)

-- | Build status.
data Status = MkStatus
  { -- | All packages in the output.
    allPkgs :: Set Package,
    -- | Packages that have started building. This will generally include
    -- packages that have completed building as well.
    buildStarted :: Set Package,
    -- | Packages that have completed building.
    completed :: Set Package
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup Status where
  MkStatus x1 x2 x3 <> MkStatus y1 y2 y3 =
    MkStatus (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance Monoid Status where
  mempty = MkStatus mempty mempty mempty

-- | Parses a status.
parseStatus :: ByteString -> Status
parseStatus = F.foldMap' go . C8.lines
  where
    go txt = case BS.stripPrefix " - " txt of
      Just rest -> mkPkg (BS.takeWhile (/= 32) rest)
      Nothing -> case BS.stripPrefix "Building" txt of
        Just rest -> mkBuilding (takeSkipLeadingSpc rest)
        Nothing -> case BS.stripPrefix "Completed" txt of
          Just rest -> mkCompleted (takeSkipLeadingSpc rest)
          Nothing -> mempty

    takeSkipLeadingSpc = BS.takeWhile (/= 32) . BS.dropWhile (== 32)

mkPkg :: ByteString -> Status
mkPkg lib =
  MkStatus
    { allPkgs = Set.singleton $ MkPackage lib,
      buildStarted = mempty,
      completed = mempty
    }

mkBuilding :: ByteString -> Status
mkBuilding lib =
  MkStatus
    { allPkgs = mempty,
      buildStarted = Set.singleton $ MkPackage lib,
      completed = mempty
    }

mkCompleted :: ByteString -> Status
mkCompleted lib =
  MkStatus
    { allPkgs = mempty,
      buildStarted = mempty,
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

-- | Formats the status.
formatStatus :: FormatStyle -> Status -> Text
formatStatus style status =
  UTF8.decodeUtf8Lenient $
    BSL.toStrict $
      BSB.toLazyByteString $
        formatAll
          style
          toBuild
          building
          completed
  where
    toBuild =
      status.allPkgs
        \\ (status.buildStarted `Set.union` status.completed)
    building = status.buildStarted \\ status.completed
    completed = status.completed

formatAll ::
  FormatStyle ->
  Set Package ->
  Set Package ->
  Set Package ->
  Builder
formatAll style toBuildS buildingS completedS = final
  where
    final =
      mconcat $
        concatter
          toBuildBuilders
          buildingBuilders
          completedBuilders

    (formatter, concatter) = case style of
      FormatNl -> (formatNewlines, concatNewlines)
      FormatNlTrunc height -> (formatNewlines, concatCompact height)
      FormatInl width -> (formatCompact width, concatNewlines)
      FormatInlTrunc height width -> (formatCompact width, concatCompact height)

    concatCompact height as bs cs =
      let (h1, bs') = takeCount height bs
          hEach = h1 `div` 2
          as' = takeTrunc hEach as
          cs' = takeTrunc hEach cs
       in concatNewlines as' bs' cs'

    -- NOTE: We do the coloring here since the "safe" way to color functions,
    -- i.e. the color function, operates on a single string type, not a list.
    concatNewlines as bs cs =
      mconcat
        [ [Pretty.magenta],
          as,
          [Pretty.endCode, "\n", "\n", Pretty.yellow],
          bs,
          [Pretty.endCode, "\n", "\n", Pretty.green],
          cs,
          [Pretty.endCode]
        ]

    toBuildL = Set.toList toBuildS
    toBuildBuilders =
      let bs = formatter toBuildL
       in "To Build: " <> (BSB.intDec $ Set.size toBuildS) : bs

    buildingL = Set.toList buildingS
    buildingBuilders =
      let bs = formatter buildingL
       in "Building: " <> (BSB.intDec $ Set.size buildingS) : bs

    completedL = Set.toList completedS
    completedBuilders =
      let bs = formatter completedL
       in "Completed: " <> (BSB.intDec $ Set.size completedS) : bs

formatNewlines :: [Package] -> [Builder]
formatNewlines = L.reverse . foldl' go []
  where
    go acc p = prependNewline (BSB.byteString p.unPackage) : acc

formatCompact :: Natural -> [Package] -> [Builder]
formatCompact width = L.reverse . fmap fst . foldl' go []
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
    go (!cnt, zs) (x : xs) = go (cnt - 1, (x : zs)) xs

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
