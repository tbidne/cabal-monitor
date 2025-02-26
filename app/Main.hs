module Main (main) where

import FileSystem.OsPath qualified as OsPath
import Monitor qualified
import System.Environment (getArgs)
import System.Exit (die)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  args <- getArgs
  path <- case args of
    [p] -> OsPath.encodeThrowM p
    other ->
      die $ "Expected a single arg for the file, received: " ++ show other

  Monitor.monitorBuild path
