module Main (main) where

import Effectful (runEff)
import Effectful.Concurrent qualified as CC
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.Optparse.Static qualified as EOA
import Monitor qualified
import Monitor.Logger qualified as Logger
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
        . FR.runFileReader
        . EOA.runOptparse
