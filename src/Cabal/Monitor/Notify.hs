module Cabal.Monitor.Notify
  ( parseSystemText,
    systemMeta,
    NotifyAction (..),
    parseActionText,
    actionMeta,
  )
where

import Cabal.Monitor.Config.Data (WithDisabled (Disabled, With))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful.Notify.Dynamic
  ( NotifySystem
      ( NotifySystemAppleScript,
        NotifySystemDBus,
        NotifySystemNotifySend,
        NotifySystemWindows
      ),
  )

parseSystemText :: (MonadFail m) => Text -> m NotifySystem
parseSystemText = \case
  "apple-script" -> pure NotifySystemAppleScript
  "dbus" -> pure NotifySystemDBus
  "notify-send" -> pure NotifySystemNotifySend
  "windows" -> pure NotifySystemWindows
  other ->
    fail $
      "Expected "
        ++ actionMeta
        ++ ", received: "
        ++ T.unpack other

systemMeta :: String
systemMeta = "(apple-script | dbus | notify-send | windows | off)"

data NotifyAction
  = NotifyActionStateChange
  | NotifyActionFinish
  | NotifyActionAll
  deriving stock (Eq, Show)

parseActionText :: (MonadFail m) => Text -> m (WithDisabled NotifyAction)
parseActionText = \case
  "state" -> pure $ With NotifyActionStateChange
  "final" -> pure $ With NotifyActionFinish
  "all" -> pure $ With NotifyActionAll
  "off" -> pure Disabled
  other ->
    fail $
      "Expected "
        ++ actionMeta
        ++ ", received: "
        ++ T.unpack other

actionMeta :: String
actionMeta = "(all | final | state | off)"
