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
import Data.Set (Set, (\\))
import Data.Set qualified as Set
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
import Monitor.Args (Args (compact, filePath, period))
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
      readPrintStatus r args.compact args.filePath
      CC.threadDelay period_ms

    period_ms = 1_000_000 * (fromMaybe 5 args.period)

readPrintStatus ::
  ( FileReader :> es,
    HasCallStack,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  r ->
  Maybe Int ->
  OsPath ->
  Eff es ()
readPrintStatus region mLineLen path = do
  formatted <- readFormattedStatus mLineLen path
  Logger.logRegion LogModeSet region formatted

readFormattedStatus ::
  ( FileReader :> es,
    HasCallStack,
    Terminal :> es
  ) =>
  Maybe Int ->
  OsPath ->
  Eff es Text
readFormattedStatus mUserLineLen path = do
  status <- readStatus path

  mLineLen <- case mUserLineLen of
    -- 1. User specified compact; use it.
    Just lineLine -> pure (Just lineLine)
    -- 2. User did not specify compact; use heuristics.
    Nothing -> do
      eResult <- Ex.trySync $ Term.getTerminalSize
      pure $ case eResult of
        -- 2.1. Attempting to find the terminal size failed. Use the default
        -- strategy.
        Left _ -> Nothing
        Right sz -> do
          -- Number of packages + 2 per 'stanza' (header and newline):
          --  - To Build
          --  - Building
          --  - Completed
          --  - Timer
          let numDefLines = 8 + length status.allLibs

          if numDefLines < sz.height
            -- 2.2. Normal, non-compact format fits in the vertical space;
            --      use it.
            then Nothing
            -- 2.3. Does not fit in vertical space. Use compact, with length
            --      determined by terminal size.
            else Just (sz.width - 1)

  pure $ formatStatus mLineLen status

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
    State.evalState @Natural 0 $
      forever $
        do
          CC.threadDelay 1_000_000
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

formatStatus :: Maybe Int -> Status -> Text
formatStatus mLineLen status =
  UTF8.unsafeDecodeUtf8 $
    BSL.toStrict $
      BSB.toLazyByteString $
        mconcat
          [ "To Build: " <> numToBuildStr <> toBuildStr,
            "\n\nBuilding: " <> numBuildingStr <> buildingStr,
            "\n\nCompleted: " <> numCompletedStr <> completedStr,
            "\n"
          ]
  where
    numCompletedStr = showtlb numCompleted
    (completedStr, numCompleted) = fmtMCompact mLineLen status.completed

    numBuildingStr = showtlb numBuilding
    (buildingStr, numBuilding) =
      fmtMCompact mLineLen (status.building \\ status.completed)

    numToBuildStr = showtlb numToBuild
    (toBuildStr, numToBuild) = fmtMCompact mLineLen toBuild
    toBuild =
      status.allLibs
        \\ (status.building `Set.union` status.completed)

    fmtMCompact :: Maybe Int -> Set Package -> (Builder, Int)
    fmtMCompact Nothing = Set.foldl' fmtBuildDefault ("", 0)
    fmtMCompact (Just lineLen) =
      (\(x, _, y) -> (x, y))
        -- HACK: Set the initial count to len + 1 so that we are guaranteed to
        -- start with a newline.
        . Set.foldl' fmtBuildCompact ("", lineLen + 1, 0)
      where
        fmtBuildCompact :: (Builder, Int, Int) -> Package -> (Builder, Int, Int)
        fmtBuildCompact (acc, !currLen, !n) p =
          let bs = p.unPackage
              bsLen = BS.length bs
              b = BSB.byteString bs
              newLen = bsLen + currLen + newPkgIdent
              newlineIdent = 4
              newPkgIdent = 2
           in if newLen + 1 > lineLen
                then (acc <> "\n  - " <> b, bsLen + newlineIdent, n + 1)
                else (acc <> ", " <> b, newLen, n + 1)

    fmtBuildDefault :: (Builder, Int) -> Package -> (Builder, Int)
    fmtBuildDefault (acc, !n) p = (acc <> "\n  - " <> BSB.byteString p.unPackage, n + 1)

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
