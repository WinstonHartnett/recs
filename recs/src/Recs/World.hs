module Recs.World where

import GHC.Base (Any)
import Recs.Archetype (Archetypes)
import Recs.Core
import Recs.EntityInfo (EntityInfo)
import Recs.TypeInfo
import Recs.Utils
import Control.Monad.Trans (MonadIO)

newtype Globals = MkGlobals {unGlobals :: GIOVector Any}

data World = MkWorld
  { archetypes :: Archetypes
  , globals :: Globals
  , entityInfo :: EntityInfo
  , typeInfo :: TypeInfo
  }

-- | System-local state.
--
-- = World Sharding
--
-- The main 'World' is farmed out to systems for local modifications, which are
-- merged back into the main 'World' upon System conclusion.
data SystemState = MkSystemState
  { commands :: Int
  }

mergeSystemStateIntoWorld :: MonadIO m => SystemState -> World -> m World
mergeSystemStateIntoWorld ss w = do
  undefined
