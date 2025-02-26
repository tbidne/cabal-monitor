module Main (main) where

import Monitor qualified
import Monitor.Args qualified as Args

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  args <- Args.getArgs
  Monitor.monitorBuild args
