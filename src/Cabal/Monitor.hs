module Cabal.Monitor
  ( -- * High level
    runMonitor,

    -- * Low level
    monitorBuild,
    readFormattedStatus,
  )
where

import Cabal.Monitor.Args (Args (filePath, height, period, width))
import Cabal.Monitor.Args qualified as Args
import Cabal.Monitor.Logger (LogMode (LogModeSet), RegionLogger)
import Cabal.Monitor.Logger qualified as Logger
import Cabal.Monitor.Pretty qualified as Pretty
import Cabal.Monitor.Status
  ( FormatStyle (FormatInl, FormatInlTrunc, FormatNl, FormatNlTrunc),
    Status (allPkgs),
  )
import Cabal.Monitor.Status qualified as Status
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
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
import GHC.Natural (Natural)
import System.Console.Regions (RegionLayout (Linear))

-- | Parses CLI args and runs the monitor loop.
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

-- | Monitors a build.
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

-- | Reads the build file and prints the formatted status.
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
  -- | Path to file to monitor.
  OsPath ->
  Eff es ()
readPrintStatus region mHeight mWidth path = do
  formatted <- readFormattedStatus mHeight mWidth path
  Logger.logRegion LogModeSet region formatted

-- | Reads the build file and formats the textual output.
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
              neededLines = int2Nat $ length status.allPkgs

          if neededLines < availPkgLines
            -- 2.2. Normal, non-compact format fits in the vertical space;
            --      use it.
            then FormatNl
            -- 2.3. Does not fit in vertical space. Use compact, with length
            --      determined by terminal size.
            else FormatInlTrunc (sz.height - 1) (sz.width - 1)

  pure $ Status.formatStatus style status

readStatus ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es Status
readStatus path = do
  contents <- FR.readBinaryFile path
  pure $ Status.parseStatus contents

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
      (\t -> Pretty.color Pretty.Blue ("\nTimer: " <> t))
        . T.pack
        . Rel.formatSeconds Rel.defaultFormat
        $ s

monus :: Natural -> Natural -> Natural
monus x y
  | x < y = 0
  | otherwise = x - y

nat2Int :: Natural -> Int
nat2Int = fromIntegral

int2Nat :: Int -> Natural
int2Nat = fromIntegral
