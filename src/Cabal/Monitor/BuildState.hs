module Cabal.Monitor.BuildState
  ( BuildState (..),
    mkNewBuildState,
    mkBuildState,
  )
where

import Cabal.Monitor.BuildStatus
  ( BuildStatus (building, completed, toBuild),
    BuildStatusFinal,
  )
import Data.Set qualified as Set

-- | Build state.
data BuildState
  = BuildWaiting
  | Building
  | BuildComplete
  deriving stock (Eq, Show)

-- | Derives the new state from the status, returns a boolean that is
-- True iff the state changed.
mkNewBuildState :: BuildState -> BuildStatusFinal -> (BuildState, Bool)
mkNewBuildState prevState status = (newState, newState /= prevState)
  where
    newState = mkBuildState status

-- | Derives the state from the status.
mkBuildState :: BuildStatusFinal -> BuildState
mkBuildState status
  -- 1. Zeroes across the board: waiting
  | noneToBuild && noneBuilding && noneCompleted = BuildWaiting
  -- 2. Completed is the only non-zero set: completed
  | noneToBuild && noneBuilding = BuildComplete
  -- 3. O/w there must be some packages left in toBuild or Building: building
  | otherwise = Building
  where
    noneToBuild = Set.null status.toBuild
    noneBuilding = Set.null status.building
    noneCompleted = Set.null status.completed
