module Main (main) where

import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.Logger qualified as Logger
import Effectful (runEff)
import Effectful.Concurrent qualified as CC
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.HandleReader.Static qualified as HR
import Effectful.FileSystem.HandleWriter.Static qualified as HW
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Notify.Dynamic qualified as Notify
import Effectful.Optparse.Static qualified as EOA
import Effectful.Process qualified as P
import Effectful.Terminal.Dynamic qualified as Term
import System.Console.Regions (ConsoleRegion)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = runner (Monitor.runMonitor ConsoleRegion Notify.NotifyEnv)
  where
    runner =
      runEff
        . CC.runConcurrent
        . P.runProcess
        . Notify.runNotify
        . Logger.runRegionLogger
        . Term.runTerminal
        . PR.runPathReader
        . HW.runHandleWriter
        . HR.runHandleReader
        . FR.runFileReader
        . FW.runFileWriter
        . EOA.runOptparse
