{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cabal.Monitor
  ( -- * High level
    runMonitor,

    -- * Low level
    BuildState (..),
    monitorBuild,
    readFormattedStatus,
    mkFormatStyleFn,
  )
where

import Cabal.Monitor.BuildState
  ( BuildState
      ( BuildComplete,
        BuildWaiting,
        Building
      ),
  )
import Cabal.Monitor.BuildState qualified as BuildState
import Cabal.Monitor.BuildStatus
  ( BuildStatusFinal,
    BuildStatusInit,
    FormatStyle (FormatInl, FormatInlTrunc, FormatNl, FormatNlTrunc),
  )
import Cabal.Monitor.BuildStatus qualified as Status
import Cabal.Monitor.Config
  ( Coloring (unColoring),
    Config (coloring, filePath, height, period, pid, width),
    Height (MkHeight),
    LocalPackages,
    Period (MkPeriod),
    SearchInfix,
    Width (MkWidth),
  )
import Cabal.Monitor.Config qualified as Config
import Cabal.Monitor.Logger (LogMode (LogModeFinish, LogModeSet), RegionLogger)
import Cabal.Monitor.Logger qualified as Logger
import Cabal.Monitor.Pretty qualified as Pretty
import Cabal.Monitor.Process (MonitorProcessC)
import Cabal.Monitor.Process qualified as Process
import Control.DeepSeq (NFData)
import Control.Exception.Utils (trySync)
import Control.Monad (forever, void, when)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
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
import Effectful.FileSystem.HandleReader.Static (HandleReader)
import Effectful.FileSystem.HandleReader.Static qualified as HR
import Effectful.FileSystem.HandleWriter.Static (HandleWriter)
import Effectful.FileSystem.HandleWriter.Static qualified as HW
import Effectful.FileSystem.PathReader.Dynamic (PathReader)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
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
import System.IO qualified as IO

-- | Parses CLI args and runs the monitor loop.
runMonitor ::
  forall r ->
  forall es.
  ( Concurrent :> es,
    FileReader :> es,
    HandleReader :> es,
    HandleWriter :> es,
    HasCallStack,
    MonitorProcessC es,
    Optparse :> es,
    PathReader :> es,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  Eff es ()
runMonitor rType = do
  config <- Config.getConfig
  monitorBuild rType config

-- (State, Status)
type MonitorState = (BuildState, BuildStatusFinal)

-- | Monitors a build.
monitorBuild ::
  forall r ->
  forall es.
  ( Concurrent :> es,
    FileReader :> es,
    HandleReader :> es,
    HandleWriter :> es,
    HasCallStack,
    MonitorProcessC es,
    PathReader :> es,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  Config ->
  Eff es ()
monitorBuild rType args = withHiddenInput $ do
  cabalPid <- Logger.displayRegions rType $ do
    Logger.withRegion @rType Linear $ \statusRegion -> do
      styleFn <- mkFormatStyleFn args.height args.width

      SState.evalState @MonitorState (BuildWaiting, mempty) $ do
        let coreProcs =
              runStatus statusRegion styleFn
                `race'` logCounter rType coloring
                `race'` drainStdinLoop

            allProcs = case args.pid of
              Nothing -> coreProcs
              Just pid ->
                coreProcs
                  `race'` Process.monitorCabalProc pid sleepSeconds

        allProcs `Ex.finally` do
          -- Need the final print here to handle CTRL-C.
          (_, finalStatus) <- SState.get @MonitorState
          -- TODO: This has been manually tested, but it would be nice to have
          -- the test suite verify that the final log is there in all situations
          -- i.e.
          --
          -- - cabal-monitor exits due to --cabal-pid finishing.
          -- - cabal-monitor manually interrupted (e.g. CTRL-C).
          let finalLog = Status.formatStatusFinal coloring (styleFn finalStatus) finalStatus
          Logger.logRegion LogModeFinish statusRegion finalLog

  -- If we get there then monitorCabalProc must have finished since none of
  -- the other threads terminate.
  Term.putTextLn $
    "\nProcess with pid "
      <> T.pack (show cabalPid)
      <> " is no longer running, terminating cabal-monitor."
  where
    sleepSeconds@(MkPeriod sleepSeconds') = args.period

    runStatus r styleFn = forever $ do
      readPrintStatus r coloring localPackages searchInfix styleFn args.filePath
      CCS.sleep sleepSeconds'

    coloring = args.coloring
    localPackages = args.localPackages
    searchInfix = args.searchInfix

type SharedState s = SState.State s

-- | Reads the build file and prints the formatted status.
readPrintStatus ::
  ( FileReader :> es,
    HasCallStack,
    PathReader :> es,
    SharedState MonitorState :> es,
    RegionLogger r :> es
  ) =>
  r ->
  -- | Color.
  Coloring ->
  -- | Local packages.
  LocalPackages ->
  -- | Search infix.
  SearchInfix ->
  -- | Style function.
  (BuildStatusFinal -> FormatStyle) ->
  -- | Path to file to monitor.
  OsPath ->
  Eff es ()
readPrintStatus region coloring localPackages searchInfix styleFn path = do
  readFormattedStatus coloring localPackages searchInfix styleFn path >>= \case
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
    SharedState MonitorState :> es
  ) =>
  -- | Color.
  Coloring ->
  -- | Local packages.
  LocalPackages ->
  -- | Search infix.
  SearchInfix ->
  -- | Style function.
  (BuildStatusFinal -> FormatStyle) ->
  OsPath ->
  Eff es (Either ReadStatusError Text)
readFormattedStatus coloring localPackages searchInfix styleFn path = do
  readStatus parseStatus path >>= \case
    Left err -> pure $ Left err
    Right statusInit -> do
      let statusFinal = Status.advancePhase statusInit
          style = styleFn statusFinal

      -- Update build state.
      let newState = BuildState.mkNewBuildState statusFinal
      SState.put (newState, statusFinal)

      pure $ Right $ Status.formatStatusFinal coloring style statusFinal
  where
    parseStatus = Status.parseStatus localPackages searchInfix

newtype ReadStatusError = PathDoesNotExist OsPath
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

readStatus ::
  ( FileReader :> es,
    HasCallStack,
    PathReader :> es
  ) =>
  (ByteString -> BuildStatusInit) ->
  OsPath ->
  Eff es (Either ReadStatusError BuildStatusInit)
readStatus parseStatus path = do
  exists <- PR.doesFileExist path
  if exists
    then do
      contents <- FR.readBinaryFile path
      pure $ Right $ parseStatus contents
    else pure $ Left $ PathDoesNotExist path

logCounter ::
  forall r ->
  forall es void.
  ( Concurrent :> es,
    HasCallStack,
    SharedState MonitorState :> es,
    RegionLogger r :> es
  ) =>
  Coloring ->
  Eff es void
logCounter rType coloring = do
  CC.threadDelay 100_000
  Logger.withRegion @rType Linear $ \r -> do
    (initState, _) <- SState.get @MonitorState
    State.evalState @BuildState initState $
      State.evalState @Natural 0 $
        forever $
          do
            CC.threadDelay 1_000_000

            prevState <- State.get @BuildState

            -- In general, we reset the timer when the state has changed.
            (buildState, _) <- SState.get @MonitorState

            -- Determine if the state changed, save new state. By saving the
            -- prev state here, we are no longer dependent on readStatus
            -- to tell us if the state changed (readStatus does change the
            -- actual state, however).
            let stateChanged = prevState /= buildState
            State.put @BuildState buildState

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
    fmtWaiting = fmtX "Waiting to start: "
    fmtBuilding = fmtX "Building: "
    fmtCompleted = fmtX "Finished in: "

    colorize =
      if coloring.unColoring
        then Pretty.color Pretty.Blue
        else id

    -- Need to put the newline _before_ the colorize, so that coloring does
    -- not bleed through.
    fmtX header = ("\n" <>) . colorize . (header <>) . fmtTime

    fmtTime =
      T.pack
        . Rel.formatSeconds Rel.defaultFormat

-- | Creates the style to use.
mkFormatStyleFn ::
  ( HasCallStack,
    Terminal :> es
  ) =>
  -- | Maybe terminal height
  Maybe Height ->
  -- | Maybe terminal width
  Maybe Width ->
  -- | Function from init to style. This is dynamic in case nothing is
  -- requested and we use heuristics to decide how to best fit the
  -- status, based on its size.
  Eff es (BuildStatusFinal -> FormatStyle)
mkFormatStyleFn mHeight mWidth = do
  case (mHeight, mWidth) of
    (Just height, Just width) -> pure $ const $ FormatInlTrunc height width
    (Just height, Nothing) -> pure $ const $ FormatNlTrunc height
    (Nothing, Just width) -> pure $ const $ FormatInl width
    (Nothing, Nothing) -> do
      eResult <- Ex.trySync Term.getTerminalSize
      pure $ case eResult of
        Left _ -> const FormatNl
        Right sz -> \statusFinal ->
          -- availPkgLines is used to tell the formatter how much vertical
          -- space it can use before truncating. This value is the
          -- terminal_height - 8, because we have 4 sections:
          --
          --   (To Build, Building, Completed, Timer)
          --
          -- And each section has a trailing newline + header.
          --
          -- Finally, we subtract 1 from availPkgLines (same as the width),
          -- to prevent the terminal from jumping to a newline, which can
          -- happen when all space is used up.
          let buffer = 4 * 2
              availPkgLines = sz.height `monus` buffer
              neededLines = int2Nat $ Status.numAllPkgs statusFinal
           in if neededLines < availPkgLines
                -- 2.2. Normal, non-compact format fits in the vertical space;
                --      use it.
                then FormatNl
                -- 2.3. Does not fit in vertical space. Use compact, with length
                --      determined by terminal size.
                else FormatInlTrunc (MkHeight availPkgLines) (MkWidth $ sz.width - 1)

-- | Subtraction, clamped to zero.
--
-- >>> 4 `monus` 2
-- 2
--
-- >>> 4 `monus` 5
-- 0
monus :: Natural -> Natural -> Natural
monus x y
  | x < y = 0
  | otherwise = x - y

int2Nat :: Int -> Natural
int2Nat = fromIntegral

withHiddenInput ::
  ( HasCallStack,
    HandleReader :> es,
    HandleWriter :> es
  ) =>
  Eff es a ->
  Eff es a
withHiddenInput m = Ex.bracket hideInput unhideInput (const m)
  where
    -- Note that this may not work on windows, if we ever want that.
    --
    -- - https://stackoverflow.com/questions/15848975/preventing-input-characters-appearing-in-terminal
    -- - https://hackage.haskell.org/package/echo
    hideInput = do
      buffMode <- HR.hGetBuffering IO.stdin
      echoMode <- HR.hGetEcho IO.stdin
      HW.hSetBuffering IO.stdin HW.NoBuffering
      HW.hSetEcho IO.stdin False
      pure (buffMode, echoMode)

    unhideInput (buffMode, echoMode) = do
      HW.hSetBuffering IO.stdin buffMode
      HW.hSetEcho IO.stdin echoMode

drainStdinLoop ::
  forall es void.
  ( Concurrent :> es,
    HasCallStack,
    HandleReader :> es
  ) =>
  Eff es void
drainStdinLoop = go
  where
    go = forever $ do
      drainStdin
      CCS.sleep 60

drainStdin :: (HasCallStack, HandleReader :> es) => Eff es ()
drainStdin =
  void $
    trySync $
      HR.hIsClosed IO.stdin
        >>= \case
          True -> pure ()
          False ->
            HR.hIsReadable IO.stdin >>= \case
              False -> pure ()
              True -> void $ HR.hGetNonBlocking IO.stdin 1_000

race' :: (Concurrent :> es) => Eff es a -> Eff es a -> Eff es a
race' mx my = Async.race mx my <&> either id id
