module Monitor
  ( -- * High level
    runMonitor,

    -- * Low level

    -- ** Type
    Status (..),
    Package (..),

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
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
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
import GHC.Generics (Generic)
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
  formatted <- readFormattedStatus path
  Logger.logRegion LogModeSet region formatted

readFormattedStatus ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es Text
readFormattedStatus path = do
  status <- readStatus path
  pure $ formatStatus status

newtype Package = MkPackage {unPackage :: ByteString}
  deriving stock (Eq, Generic, Ord, Show)
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

formatStatus :: Status -> Text
formatStatus status =
  UTF8.unsafeDecodeUtf8 $
    BSL.toStrict $
      BSB.toLazyByteString $
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
        fmap (\p -> "\n - " <> BSB.byteString p.unPackage) xs

    showtlb :: Int -> Builder
    showtlb = BSB.intDec

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

showt :: (Show a) => a -> Text
showt = T.pack . show
