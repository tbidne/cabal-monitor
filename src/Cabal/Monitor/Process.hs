{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cabal.Monitor.Process
  ( MonitorProcessC,
    monitorCabalProc,
    runMonitorProcessC,
  )
where

import Control.Exception.Utils qualified as Ex
import Data.Kind (Constraint)
import Effectful (Eff, Effect, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (HasCallStack)
import GHC.Natural (Natural)

#if !WINDOWS
import Control.Monad (unless)
import Data.Bits (toIntegralSized)
import Data.Functor ((<&>))
import Effectful qualified as Eff
import Effectful.Concurrent.Static qualified as CCS
import Effectful.Posix.Signals.Static (PosixSignals)
import Effectful.Posix.Signals.Static qualified as Signals
import System.Posix.Types (ProcessID)
#endif

-- | Compat alias for windows/posix-specific required constraints.
type MonitorProcessC :: [Effect] -> Constraint

-- | Monitors the given pid, exiting when the pid is no longer running.
monitorCabalProc ::
  ( Concurrent :> es,
    HasCallStack,
    MonitorProcessC es
  ) =>
  -- | pid.
  Natural ->
  -- | Sleep seconds.
  Natural ->
  Eff es Natural
#if WINDOWS
monitorCabalProc _ _ =
  Ex.throwString "Monitoring the cabal pid is not supported on windows."

type MonitorProcessC es = ()

-- | Handler for 'MonitorProcessC'.
runMonitorProcessC :: Eff es a -> Eff es a
runMonitorProcessC = id

#else

monitorCabalProc pid sleepSeconds = do
  processId <- case toIntegralSized @Natural @ProcessID pid of
    Nothing ->
      Ex.throwString $ "Failed converting pid " ++ show pid
    Just p -> pure p

  let go = do
        CCS.sleep sleepSeconds
        isRunning <- checkPid processId
        if isRunning
          then go
          else pure pid

  hasStarted <- checkPid processId
  unless hasStarted (Ex.throwString $ "Pid " ++ show pid ++ " not found.")

  go

checkPid :: (HasCallStack, MonitorProcessC es) => ProcessID -> Eff es Bool
checkPid pid = do
  Ex.trySync (Signals.signalProcess 0 pid) <&> \case
    Left _ -> False
    Right _ -> True

type MonitorProcessC es = (PosixSignals :> es)

-- | Handler for 'MonitorProcessC'.
runMonitorProcessC :: (Eff.IOE :> es) => Eff (PosixSignals : es) a -> Eff es a
runMonitorProcessC = Signals.runPosixSignals
#endif
