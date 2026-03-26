module Cabal.Monitor.Config
  ( -- * Config
    Config (..),
    getConfig,

    -- * Base Types
    Coloring (..),
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
    Height (MkHeight, unHeight),
    LocalPackages (MkLocalPackages, unLocalPackages),
    Period (MkPeriod, unPeriod),
    Pid (MkPid, unPid),
    SearchInfix (MkSearchInfix, unSearchInfix),
    Width (MkWidth, unWidth),
    (<|.|>),
  )
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

data Config = MkConfig
  { coloring :: Coloring,
    filePath :: OsPath,
    height :: Maybe Height,
    localPackages :: LocalPackages,
    period :: Period,
    pid :: Maybe Pid,
    searchInfix :: SearchInfix,
    width :: Maybe Width
  }
  deriving stock (Eq, Show)

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
        filePath = args.filePath,
        height = args.height <|> (toml >>= (.height)),
        localPackages = args.localPackages <|.|> (toml >>= (.localPackages)),
        period = args.period <|.|> (toml >>= (.period)),
        pid = args.pid,
        searchInfix = args.searchInfix <|.|> (toml >>= (.searchInfix)),
        width = args.width <|> (toml >>= (.width))
      }
