module Cabal.Monitor.Config
  ( -- * Config
    Config (..),
    getConfig,

    -- * Base Types
    Coloring (..),
    Debug (..),
    Height (..),
    LocalPackages (..),
    Period (..),
    Pid (..),
    SearchInfix (..),
    Width (..),
  )
where

import Cabal.Monitor.Config.Args
  ( Args
      ( coloring,
        filePath,
        height,
        period,
        pid,
        width
      ),
  )
import Cabal.Monitor.Config.Args qualified as Args
import Cabal.Monitor.Config.Data
  ( Coloring (MkColoring, unColoring),
    Debug (MkDebug, unDebug),
    Height (MkHeight, unHeight),
    LocalPackages (MkLocalPackages, unLocalPackages),
    Period (MkPeriod, unPeriod),
    Pid (MkPid, unPid),
    SearchInfix (MkSearchInfix, unSearchInfix),
    Width (MkWidth, unWidth),
    (<|.|>),
  )
import Cabal.Monitor.Config.Data qualified as Data
import Cabal.Monitor.Config.Toml
  ( Toml
      ( coloring,
        height,
        localPackages,
        period,
        searchInfix,
        width
      ),
  )
import Cabal.Monitor.Config.Toml qualified as Toml
import Control.Applicative ((<|>))
import Effectful (Eff, type (:>))
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.FileSystem.FileReader.Static (FileReader)
import Effectful.FileSystem.PathReader.Dynamic (PathReader)
import Effectful.Optparse.Static (Optparse)
import FileSystem.OsPath (OsPath)

-- | Config used by cabal-monitor.
data Config = MkConfig
  { -- | Whether to color the logs.
    coloring :: Coloring,
    -- | Debug flag.
    debug :: Debug,
    -- | Path to file to monitor.
    filePath :: OsPath,
    -- | Possible terminal height.
    height :: Maybe Height,
    -- | Whether to monitor output for local packages (requires extra logic).
    localPackages :: LocalPackages,
    -- | How often to read the status, in seconds.
    period :: Period,
    -- | Pid of the process we are monitoring, for exiting automatically.
    pid :: Maybe Pid,
    -- | Whether to search logs for infix patterns, for more flexibility at
    -- the cost of performance.
    searchInfix :: SearchInfix,
    -- | Possible terminal width.
    width :: Maybe Width
  }
  deriving stock (Eq, Show)

-- | Combines CLI args and TOML config to produce a final 'Config'.
getConfig ::
  ( FileReader :> es,
    HasCallStack,
    Optparse :> es,
    PathReader :> es
  ) =>
  Eff es Config
getConfig = do
  args <- Args.getArgs
  toml <- Toml.getTomlConfig args.configPath
  pure $
    MkConfig
      { coloring = args.coloring <|.|> (toml >>= (.coloring)),
        debug = Data.toMaybe args.debug,
        filePath = args.filePath,
        height = args.height <|> (toml >>= (.height)),
        localPackages = args.localPackages <|.|> (toml >>= (.localPackages)),
        period = args.period <|.|> (toml >>= (.period)),
        pid = args.pid,
        searchInfix = args.searchInfix <|.|> (toml >>= (.searchInfix)),
        width = args.width <|> (toml >>= (.width))
      }
