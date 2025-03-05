module Monitor
  ( -- * High level
    runMonitor,

    -- * Low level

    -- ** Type
    Status (..),
    Package (..),
    FormatStyle (..),

    -- ** Functions
    monitorBuild,
    readFormattedStatus,
    parseStatus,
    formatStatus,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (foldMap')
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Relative qualified as Rel
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
import Effectful.Concurrent.Async qualified as Async
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.Exception qualified as Ex
import Effectful.FileSystem.FileReader.Static (FileReader)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.Optparse.Static (Optparse)
import Effectful.State.Static.Local qualified as State
import Effectful.Terminal.Dynamic (Terminal)
import Effectful.Terminal.Dynamic qualified as Term
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as UTF8
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Monitor.Args (Args (filePath, height, period, width))
import Monitor.Args qualified as Args
import Monitor.Logger (LogMode (LogModeSet), RegionLogger)
import Monitor.Logger qualified as Logger
import System.Console.Regions (RegionLayout (Linear))

runMonitor ::
  forall r ->
  forall es void.
  ( Concurrent :> es,
    FileReader :> es,
    HasCallStack,
    Optparse :> es,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  Eff es void
runMonitor rType = do
  args <- Args.getArgs
  monitorBuild rType args

monitorBuild ::
  forall r ->
  forall es void.
  ( Concurrent :> es,
    FileReader :> es,
    HasCallStack,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  Args ->
  Eff es void
monitorBuild rType args =
  Logger.displayRegions rType $ do
    eResult <-
      Async.race
        runStatus
        (logCounter rType)

    case eResult of
      Left e -> pure e
      Right x -> pure x
  where
    runStatus = Logger.withRegion @rType Linear $ \r -> forever $ do
      readPrintStatus r args.height args.width args.filePath
      CC.threadDelay period_ms

    -- TODO: We should use the sleepSecond which is in terms of Natural.
    period_ms = 1_000_000 * (nat2Int $ fromMaybe 5 args.period)

readPrintStatus ::
  ( FileReader :> es,
    HasCallStack,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  r ->
  -- | Maybe terminal height
  Maybe Natural ->
  -- | Maybe terminal width
  Maybe Natural ->
  OsPath ->
  Eff es ()
readPrintStatus region mHeight mWidth path = do
  formatted <- readFormattedStatus mHeight mWidth path
  Logger.logRegion LogModeSet region formatted

readFormattedStatus ::
  ( FileReader :> es,
    HasCallStack,
    Terminal :> es
  ) =>
  -- | Maybe terminal height
  Maybe Natural ->
  -- | Maybe terminal width
  Maybe Natural ->
  OsPath ->
  Eff es Text
readFormattedStatus mHeight mWidth path = do
  status <- readStatus path

  style <- case (mHeight, mWidth) of
    (Just height, Just width) -> pure $ FormatInlTrunc height width
    (Just height, Nothing) -> pure $ FormatNlTrunc height
    (Nothing, Just width) -> pure $ FormatInl width
    (Nothing, Nothing) -> do
      eResult <- Ex.trySync $ Term.getTerminalSize
      pure $ case eResult of
        Left _ -> FormatNl
        Right sz -> do
          -- - 4 (To Build, Building, Completed, Timer) stanzas with a header
          --   and trailing newline --> 4 * 2 == 8
          --
          --   --> 4 * 2 == 8
          let availPkgLines = sz.height `monus` 8
              neededLines = int2Nat $ length status.allLibs

          if neededLines < availPkgLines
            -- 2.2. Normal, non-compact format fits in the vertical space;
            --      use it.
            then FormatNl
            -- 2.3. Does not fit in vertical space. Use compact, with length
            --      determined by terminal size.
            else FormatInlTrunc (sz.height - 1) (sz.width - 1)

  pure $ formatStatus style status

newtype Package = MkPackage {unPackage :: ByteString}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (IsString)
  deriving anyclass (NFData)

data Status = MkStatus
  { allLibs :: Set Package,
    building :: Set Package,
    completed :: Set Package
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup Status where
  MkStatus x1 x2 x3 <> MkStatus y1 y2 y3 =
    MkStatus (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance Monoid Status where
  mempty = MkStatus mempty mempty mempty

readStatus ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es Status
readStatus path = do
  contents <- FR.readBinaryFile path
  pure $ parseStatus $ C8.lines contents

logCounter ::
  forall r ->
  forall es void.
  ( Concurrent :> es,
    HasCallStack,
    RegionLogger r :> es
  ) =>
  Eff es void
logCounter rType = do
  CC.threadDelay 100_000
  Logger.withRegion @rType Linear $ \r ->
    State.evalState @Natural 0 $
      forever $
        do
          CC.threadDelay 1_000_000
          -- IDEA: Could use shared state and detect a file change, in which
          -- case we reset the timer?
          State.modify @Natural (\(!x) -> x + 1)
          elapsed <- State.get
          Logger.logRegion
            LogModeSet
            r
            (fmtTime elapsed)
  where
    fmtTime s =
      (\t -> "\nTimer: " <> t)
        . T.pack
        . Rel.formatSeconds Rel.defaultFormat
        $ s

parseStatus :: [ByteString] -> Status
parseStatus = foldMap' go
  where
    go txt = case BS.stripPrefix " - " txt of
      Just rest -> mkLib (BS.takeWhile (/= 32) rest)
      Nothing -> case BS.stripPrefix "Building" txt of
        Just rest -> mkBuilding (takeSkipLeadingSpc rest)
        Nothing -> case BS.stripPrefix "Completed" txt of
          Just rest -> mkCompleted (takeSkipLeadingSpc rest)
          Nothing -> mempty

    takeSkipLeadingSpc = BS.takeWhile (/= 32) . BS.dropWhile (== 32)

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

formatStatus :: FormatStyle -> Status -> Text
formatStatus style status =
  UTF8.unsafeDecodeUtf8 $
    BSL.toStrict $
      BSB.toLazyByteString $
        formatAll
          style
          toBuild
          building
          completed
  where
    toBuild =
      status.allLibs
        \\ (status.building `Set.union` status.completed)
    building = status.building \\ status.completed
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

    concatNewlines as bs cs =
      mconcat
        [ as,
          ["\n", "\n"],
          bs,
          ["\n", "\n"],
          cs
        ]

    concatCompact height as bs cs =
      let (h1, bs') = takeCount height bs
          hEach = h1 `div` 2
          as' = takeTrunc hEach as
          cs' = takeTrunc hEach cs
       in mconcat
            [ as',
              ["\n", "\n"],
              bs',
              ["\n", "\n"],
              cs'
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

mkLib :: ByteString -> Status
mkLib lib =
  MkStatus
    { allLibs = Set.singleton $ MkPackage lib,
      building = mempty,
      completed = mempty
    }

mkBuilding :: ByteString -> Status
mkBuilding lib =
  MkStatus
    { allLibs = mempty,
      building = Set.singleton $ MkPackage lib,
      completed = mempty
    }

mkCompleted :: ByteString -> Status
mkCompleted lib =
  MkStatus
    { allLibs = mempty,
      building = mempty,
      completed = Set.singleton $ MkPackage lib
    }

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

monus :: Natural -> Natural -> Natural
monus x y
  | x < y = 0
  | otherwise = x - y

nat2Int :: Natural -> Int
nat2Int = fromIntegral

int2Nat :: Int -> Natural
int2Nat = fromIntegral
