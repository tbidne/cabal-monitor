module Main (main) where

import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.Logger qualified as Logger
import Effectful (runEff)
import Effectful.Concurrent qualified as CC
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.PathReader.Static qualified as PR
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
        . Logger.runRegionLogger
        . Term.runTerminal
        . PR.runPathReader
        . FR.runFileReader
        . EOA.runOptparse
