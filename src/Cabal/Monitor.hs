module Cabal.Monitor
  ( -- * High level
    runMonitor,

    -- * Low level
    BuildState (..),
    monitorBuild,
    readFormattedStatus,
  )
where

import Cabal.Monitor.Args
  ( Args (coloring, filePath, height, period, width),
    Coloring (MkColoring, unColoring),
  )
import Cabal.Monitor.Args qualified as Args
import Cabal.Monitor.BuildState
  ( BuildState
      ( BuildComplete,
        BuildWaiting,
        Building
      ),
  )
import Cabal.Monitor.BuildState qualified as BuildState
import Cabal.Monitor.BuildStatus
  ( BuildStatusInit,
    FormatStyle (FormatInl, FormatInlTrunc, FormatNl, FormatNlTrunc),
  )
import Cabal.Monitor.BuildStatus qualified as Status
import Cabal.Monitor.Logger (LogMode (LogModeSet), RegionLogger)
import Cabal.Monitor.Logger qualified as Logger
import Cabal.Monitor.Pretty qualified as Pretty
import Control.DeepSeq (NFData)
import Control.Monad (forever, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Relative qualified as Rel
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.Static qualified as CCS
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.Exception qualified as Ex
import Effectful.FileSystem.FileReader.Static (FileReader)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.PathReader.Static (PathReader)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.Optparse.Static (Optparse)
import Effectful.State.Static.Local qualified as State
import Effectful.State.Static.Shared qualified as SState
import Effectful.Terminal.Dynamic (Terminal)
import Effectful.Terminal.Dynamic qualified as Term
import FileSystem.OsPath (OsPath)
import FileSystem.OsPath qualified as OsPath
import GHC.Generics (Generic)
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
    PathReader :> es,
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
    PathReader :> es,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  Args ->
  Eff es void
monitorBuild rType args =
  Logger.displayRegions rType $ do
    SState.evalState (BuildWaiting, False) $ do
      eResult <-
        Async.race
          runStatus
          (logCounter rType coloring)

      case eResult of
        Left e -> pure e
        Right x -> pure x
  where
    runStatus = do
      let sleepSeconds = fromMaybe 5 args.period
      Logger.withRegion @rType Linear $ \r -> forever $ do
        readPrintStatus r coloring args.height args.width args.filePath
        CCS.sleep sleepSeconds

    coloring = fromMaybe (MkColoring True) args.coloring

type SharedState s = SState.State s

-- | Reads the build file and prints the formatted status.
readPrintStatus ::
  ( FileReader :> es,
    HasCallStack,
    PathReader :> es,
    SharedState (BuildState, Bool) :> es,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  r ->
  -- | Color.
  Coloring ->
  -- | Maybe terminal height
  Maybe Natural ->
  -- | Maybe terminal width
  Maybe Natural ->
  -- | Path to file to monitor.
  OsPath ->
  Eff es ()
readPrintStatus region coloring mHeight mWidth path = do
  readFormattedStatus coloring mHeight mWidth path >>= \case
    Left (PathDoesNotExist p) ->
      Logger.logRegion
        LogModeSet
        region
        $ mconcat
          [ "Build file does not exist at path: '",
            T.pack $ OsPath.decodeLenient p,
            "'"
          ]
    Right formatted -> Logger.logRegion LogModeSet region formatted

-- | Reads the build file and formats the textual output.
readFormattedStatus ::
  ( FileReader :> es,
    HasCallStack,
    PathReader :> es,
    SharedState (BuildState, Bool) :> es,
    Terminal :> es
  ) =>
  -- | Color.
  Coloring ->
  -- | Maybe terminal height
  Maybe Natural ->
  -- | Maybe terminal width
  Maybe Natural ->
  OsPath ->
  Eff es (Either ReadStatusError Text)
readFormattedStatus coloring mHeight mWidth path = do
  readStatus path >>= \case
    Left err -> pure $ Left err
    Right statusInit -> do
      let statusFinal = Status.advancePhase statusInit

      -- Update build state.
      SState.modify
        ( \(prevState, _) ->
            BuildState.mkNewBuildState prevState statusFinal
        )

      style <- case (mHeight, mWidth) of
        (Just height, Just width) -> pure $ FormatInlTrunc height width
        (Just height, Nothing) -> pure $ FormatNlTrunc height
        (Nothing, Just width) -> pure $ FormatInl width
        (Nothing, Nothing) -> do
          eResult <- Ex.trySync Term.getTerminalSize
          pure $ case eResult of
            Left _ -> FormatNl
            Right sz -> do
              -- availPkgLines is used to tell the formatter how much vertical
              -- space it can use before truncating. This value is the
              -- terminal_height - 4, because we have 4 sections:
              --
              --   (To Build, Building, Completed, Timer)
              --
              -- And each section has a trailing newline. Each section also
              -- has a header, but the formatter includes that in its
              -- calculations, so we do not need to take it into account here.
              --
              -- Finally, we subtract 1 from availPkgLines (same as the width),
              -- to prevent the terminal from jumping to a newline, which can
              -- happen when all space is used up.
              let availPkgLines = sz.height `monus` 4
                  neededLines = int2Nat $ Status.numAllPkgs statusInit

              if neededLines < availPkgLines
                -- 2.2. Normal, non-compact format fits in the vertical space;
                --      use it.
                then FormatNl
                -- 2.3. Does not fit in vertical space. Use compact, with length
                --      determined by terminal size.
                else FormatInlTrunc (availPkgLines - 1) (sz.width - 1)

      pure $ Right $ Status.formatStatusFinal coloring style statusFinal

newtype ReadStatusError = PathDoesNotExist OsPath
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

readStatus ::
  ( FileReader :> es,
    HasCallStack,
    PathReader :> es
  ) =>
  OsPath ->
  Eff es (Either ReadStatusError BuildStatusInit)
readStatus path = do
  exists <- PR.doesFileExist path
  if exists
    then do
      contents <- FR.readBinaryFile path
      pure $ Right $ Status.parseStatus contents
    else pure $ Left $ PathDoesNotExist path

logCounter ::
  forall r ->
  forall es void.
  ( Concurrent :> es,
    HasCallStack,
    SharedState (BuildState, Bool) :> es,
    RegionLogger r :> es
  ) =>
  Coloring ->
  Eff es void
logCounter rType coloring = do
  CC.threadDelay 100_000
  Logger.withRegion @rType Linear $ \r ->
    State.evalState @Natural 0 $
      forever $
        do
          CC.threadDelay 1_000_000

          -- In general, we reset the timer when the state has changed.
          (buildState, stateChanged) <- SState.get @(BuildState, Bool)

          case buildState of
            Building -> do
              when stateChanged (State.put @Natural 0)

              State.modify @Natural (\(!x) -> x + 1)
              elapsed <- State.get
              Logger.logRegion
                LogModeSet
                r
                (fmtBuilding elapsed)
            BuildComplete -> do
              elapsed <- State.get
              Logger.logRegion
                LogModeSet
                r
                (fmtCompleted elapsed)
            BuildWaiting -> do
              when stateChanged (State.put @Natural 0)

              State.modify @Natural (\(!x) -> x + 1)
              elapsed <- State.get
              Logger.logRegion
                LogModeSet
                r
                (fmtWaiting elapsed)
  where
    fmtWaiting = fmtX "\nWaiting to start: "
    fmtBuilding = fmtX "\nBuilding: "
    fmtCompleted = fmtX "\nFinished in: "

    colorize =
      if coloring.unColoring
        then Pretty.color Pretty.Blue
        else id

    fmtX header = colorize . (header <>) . fmtTime

    fmtTime =
      T.pack
        . Rel.formatSeconds Rel.defaultFormat

monus :: Natural -> Natural -> Natural
monus x y
  | x < y = 0
  | otherwise = x - y

int2Nat :: Int -> Natural
int2Nat = fromIntegral
