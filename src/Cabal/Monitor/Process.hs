{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cabal.Monitor.Process
  ( monitorCabalProc,
  )
where

import Cabal.Monitor.Config (Debug (unDebug), Period (MkPeriod), Pid (MkPid))
import Control.Exception (displayException)
import Control.Exception.Utils qualified as Ex
import Control.Monad (unless, when)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Static qualified as CCS
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.FileSystem.FileWriter.Static (FileWriter)
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.PathReader.Dynamic (PathReader)
import Effectful.Process (Process)
import Effectful.Process qualified as P
import FileSystem.OsPath (OsPath, osp)
import GHC.Natural (Natural)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

#if !WINDOWS
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import FileSystem.OsPath qualified as OsP
#endif

-- | Monitors the given pid, exiting when the pid is no longer running.
monitorCabalProc ::
  ( Concurrent :> es,
    FileWriter :> es,
    HasCallStack,
    PathReader :> es,
    Process :> es
  ) =>
  Debug ->
  -- | pid.
  Pid ->
  -- | Sleep seconds.
  Period ->
  Eff es Natural
monitorCabalProc debug (MkPid pid) (MkPeriod sleepSeconds) = do
  processArgs <- getProcessArgs

  let go = do
        CCS.sleep sleepSeconds
        isRunning <- checkPid debug processArgs pid
        if isRunning
          then go
          else pure pid

  hasStarted <- checkPid debug processArgs pid
  unless hasStarted (Ex.throwString $ "Pid " ++ show pid ++ " not found.")

  go

-- (Command, pidToArgs)
type ProcessArgs = (String, Natural -> [String])

checkPid ::
  ( FileWriter :> es,
    HasCallStack,
    Process :> es
  ) =>
  Debug ->
  ProcessArgs ->
  Natural ->
  Eff es Bool
checkPid debug (cmd, mkArgs) pid = do
  Ex.trySync runFindPid >>= \case
    -- Actual failure, let's just return False.
    Left ex -> do
      when debug.unDebug $ logDebug "Exception" (displayException ex)
      pure False
    -- Potentially expected failure e.g. 'kill -0' failed for some reason.
    Right x@(ExitFailure _, _, _) -> do
      when debug.unDebug $ logDebug "Failure" (show x)
      pure False
    -- Success, pid is alive.
    Right x@(ExitSuccess, out, _) -> do
      when debug.unDebug $ logDebug "Success" (show x)
      pure $ onSuccessOut out
  where
    runFindPid = P.readProcessWithExitCode cmd (mkArgs pid) "runFindPid"

    logDebug header x = do
      let msg =
            mconcat
              [ header,
                " retrieving pid ",
                T.pack $ show pid,
                ": ",
                T.pack x,
                "\n"
              ]
      FW.appendFileUtf8 processOsP msg

onSuccessOut :: String -> Bool
#if WINDOWS
-- On windows, we determine if the process is running by the output of
-- tasklist i.e. we search for the below string as an indicator that the task
-- is not running.
onSuccessOut =
  not
    . T.isInfixOf "No tasks are running which match the specified criteria"
    . T.pack
#else
-- On posix, we can use the exit code from 'kill -0' to determine if the pid
-- is running, so the output is irrelevant i.e. success is always true.
onSuccessOut = const True
#endif

getProcessArgs ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es ProcessArgs
getProcessArgs = do
#if WINDOWS
  pure ("tasklist", \pid -> ["/nh", "/fi", "pid eq " ++ show pid])
#else
  killExeStr <-
    PR.findExecutable [osp|kill|] >>= \case
      Just exe -> OsP.decodeThrowM exe
      Nothing ->
        Ex.throwString
          "Cannot find kill exe, required for --pid."
  pure (killExeStr, \pid -> ["-0", show pid])
#endif

processOsP :: OsPath
processOsP = [osp|cabal_monitor_process.log|]
