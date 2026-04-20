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
    NotifyConfig,
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
    WithDisabled (Disabled, With),
    (<.>),
  )
import Cabal.Monitor.Config.Data qualified as Data
import Cabal.Monitor.Config.Toml
  ( Toml
      ( coloring,
        height,
        localPackages,
        notifyAction,
        notifySystem,
        period,
        searchInfix,
        width
      ),
  )
import Cabal.Monitor.Config.Toml qualified as Toml
import Cabal.Monitor.Notify (NotifyAction)
import Control.Applicative ((<|>))
import Effectful (Eff, type (:>))
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.FileSystem.FileReader.Static (FileReader)
import Effectful.FileSystem.PathReader.Dynamic (PathReader)
import Effectful.Notify.Dynamic (Notify, NotifyEnv)
import Effectful.Notify.Dynamic qualified as Notify
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
    -- | Notify.
    notify :: NotifyConfig,
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
  deriving stock (Show)

type NotifyConfig = Maybe (NotifyAction, NotifyEnv)

-- | Combines CLI args and TOML config to produce a final 'Config'.
getConfig ::
  ( FileReader :> es,
    HasCallStack,
    Notify :> es,
    Optparse :> es,
    PathReader :> es
  ) =>
  Eff es Config
getConfig = do
  args <- Args.getArgs
  toml <- Toml.getTomlConfig args.configPath

  let notifySystem = case (args.notifySystem, toml >>= (.notifySystem)) of
        (Just sys, _) -> sys
        (Nothing, Just sys) -> sys
        (Nothing, Nothing) -> Notify.defaultNotifySystem

      onAct act = do
        notifySystemOs <- Notify.notifySystemToOs notifySystem
        notifyEnv <- Notify.initNotifyEnv notifySystemOs
        pure $ Just (act, notifyEnv)

  notify <- case (args.notifyAction, toml >>= (.notifyAction)) of
    (Just Disabled, _) -> pure Nothing
    (Just (With act), _) -> onAct act
    (Nothing, Just (With act)) -> onAct act
    (Nothing, _) -> pure Nothing

  pure $
    MkConfig
      { coloring = args.coloring <.> (toml >>= (.coloring)),
        debug = Data.toMaybe args.debug,
        filePath = args.filePath,
        height = args.height <|> (toml >>= (.height)),
        localPackages = args.localPackages <.> (toml >>= (.localPackages)),
        notify,
        period = args.period <.> (toml >>= (.period)),
        pid = args.pid,
        searchInfix = args.searchInfix <.> (toml >>= (.searchInfix)),
        width = args.width <|> (toml >>= (.width))
      }
