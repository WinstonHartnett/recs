{-# LANGUAGE DataKinds #-}

module Recs.System where

import Recs.Commands
import Recs.World
import Effectful
import Effectful.State.Static.Local

-- | System-local state.
data SystemState m = MkSystemState
  { commands :: !(Commands m)
  , world :: !World
  }

type System m es = (State (SystemState m) :> es, Ecs es)

runSystem :: Eff (State (SystemState m) : es) a -> Eff es (a, SystemState m)
runSystem = runState (MkSystemState undefined undefined)
