module Main (main) where

import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.Logger qualified as Logger
import Cabal.Monitor.Process qualified as Process
import Effectful (runEff)
import Effectful.Concurrent qualified as CC
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.HandleReader.Static qualified as HR
import Effectful.FileSystem.HandleWriter.Static qualified as HW
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Optparse.Static qualified as EOA
import Effectful.Terminal.Dynamic qualified as Term
import System.Console.Regions (ConsoleRegion)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = runner (Monitor.runMonitor ConsoleRegion)
  where
    runner =
      runEff
        . CC.runConcurrent
        . Process.runMonitorProcessC
        . Logger.runRegionLogger
        . Term.runTerminal
        . PR.runPathReader
        . HW.runHandleWriter
        . HR.runHandleReader
        . FR.runFileReader
        . EOA.runOptparse
