module Monitor
  ( -- * High level
    runMonitor,

    -- * Low level
    monitorBuild,
  )
where

import Control.Monad (forever)
import Data.Foldable (foldMap')
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
import Effectful.Concurrent.Async qualified as Async
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.FileSystem.FileReader.Static (FileReader)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.Optparse.Static (Optparse)
import Effectful.State.Static.Local qualified as State
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as UTF8
import Monitor.Args (Args (filePath, period))
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
    RegionLogger r :> es
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
    RegionLogger r :> es
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
      readPrintStatus r args.filePath
      CC.threadDelay period_ms

    period_ms = 1_000_000 * (fromMaybe 5 args.period)

readPrintStatus ::
  ( FileReader :> es,
    HasCallStack,
    RegionLogger r :> es
  ) =>
  r ->
  OsPath ->
  Eff es ()
readPrintStatus region path = do
  status <- readStatus path
  let formatted = fmtStatus status
  Logger.logRegion LogModeSet region formatted

newtype Package = MkPackage {unPackage :: Text}
  deriving stock (Eq, Ord, Show)

data Status = MkStatus
  { allLibs :: Set Package,
    building :: Set Package,
    completed :: Set Package
  }
  deriving stock (Eq, Show)

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
  contents <- UTF8.decodeUtf8ThrowM =<< FR.readBinaryFile path
  pure $ parseStatus $ T.lines contents

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
    State.evalState @Word 0 $
      forever $
        do
          CC.threadDelay 1_000_000
          State.modify @Word (\(!x) -> x + 1)
          elapsed <- State.get
          Logger.logRegion LogModeSet r (fmtTime elapsed)
  where
    fmtTime :: Word -> Text
    fmtTime n =
      mconcat
        [ "\nRunning: ",
          showt n,
          " second(s)"
        ]

parseStatus :: [Text] -> Status
parseStatus = foldMap' go
  where
    go txt = case T.stripPrefix " - " txt of
      Just rest -> mkLib (T.takeWhile (/= ' ') rest)
      Nothing -> case T.stripPrefix "Building" txt of
        Just rest -> mkBuilding (takeSkipLeadingSpc rest)
        Nothing -> case T.stripPrefix "Completed" txt of
          Just rest -> mkCompleted (takeSkipLeadingSpc rest)
          Nothing -> mempty

    takeSkipLeadingSpc = T.takeWhile (/= ' ') . T.dropWhile (== ' ')

fmtStatus :: Status -> Text
fmtStatus status =
  TL.toStrict $
    TLB.toLazyText $
      mconcat
        [ "Packages: " <> showtlb (length status.allLibs),
          "\nCompleted: " <> showtlb (length status.completed),
          "\nBuilding " <> numBuilding <> ": " <> fmtBuilding (Set.toList building),
          "\n"
        ]
  where
    numBuilding = showtlb (length building)
    building =
      Set.filter
        (\p -> not (p `Set.member` status.completed))
        status.building

    fmtBuilding :: [Package] -> Builder
    fmtBuilding [] = ""
    fmtBuilding xs =
      mconcat $
        fmap (\p -> "\n - " <> TLB.fromText p.unPackage) xs

    showtlb :: forall a. (Show a) => a -> Builder
    showtlb = TLB.fromString . show

mkLib :: Text -> Status
mkLib lib =
  MkStatus
    { allLibs = Set.singleton $ MkPackage lib,
      building = mempty,
      completed = mempty
    }

mkBuilding :: Text -> Status
mkBuilding lib =
  MkStatus
    { allLibs = mempty,
      building = Set.singleton $ MkPackage lib,
      completed = mempty
    }

mkCompleted :: Text -> Status
mkCompleted lib =
  MkStatus
    { allLibs = mempty,
      building = mempty,
      completed = Set.singleton $ MkPackage lib
    }

showt :: (Show a) => a -> Text
showt = T.pack . show
