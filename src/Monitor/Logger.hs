module Monitor.Logger
  ( -- * Effect
    RegionLogger (..),
    LogMode (..),
    logRegion,
    withRegion,
    displayRegions,

    -- * Handler
    runRegionLogger,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    MonadIO (liftIO),
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( interpret,
    localSeqUnliftIO,
    send,
  )
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import GHC.Stack (HasCallStack)
import System.Console.Regions (ConsoleRegion, RegionLayout)
import System.Console.Regions qualified as Regions

type RegionLogger :: Type -> Effect
data RegionLogger r :: Effect where
  LogRegion :: forall r m. LogMode -> r -> Text -> RegionLogger r m ()
  WithRegion :: forall r m a. RegionLayout -> (r -> m a) -> RegionLogger r m a
  DisplayRegions :: forall r m a. m a -> RegionLogger r m a

type instance DispatchOf (RegionLogger _) = Dynamic

instance ShowEffect (RegionLogger r) where
  showEffectCons = \case
    LogRegion {} -> "LogRegion"
    WithRegion {} -> "WithRegion"
    DisplayRegions {} -> "DisplayRegions"

data LogMode
  = LogModeSet
  | LogModeAppend
  | LogModeFinish
  deriving stock (Eq, Show)

runRegionLogger ::
  ( r ~ ConsoleRegion,
    HasCallStack,
    IOE :> es
  ) =>
  Eff (RegionLogger r : es) a ->
  Eff es a
runRegionLogger = interpret $ \env -> \case
  LogRegion m r t -> case m of
    LogModeSet -> liftIO $ Regions.setConsoleRegion r t
    LogModeAppend -> liftIO $ Regions.appendConsoleRegion r t
    LogModeFinish -> liftIO $ Regions.finishConsoleRegion r t
  WithRegion layout onRegion -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Regions.withConsoleRegion layout (runInIO . onRegion)
  DisplayRegions m ->
    localSeqUnliftIO env $ \runInIO ->
      liftIO $ Regions.displayConsoleRegions (runInIO m)

logRegion ::
  forall r es.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  LogMode ->
  r ->
  Text ->
  Eff es ()
logRegion m r = send . LogRegion @r m r

withRegion ::
  forall r es a.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  RegionLayout ->
  (r -> Eff es a) ->
  Eff es a
withRegion l = send . WithRegion @r l

displayRegions ::
  forall r ->
  forall es a.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  Eff es a ->
  Eff es a
displayRegions r = send . DisplayRegions @r
