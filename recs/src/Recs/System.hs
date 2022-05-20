{-# LANGUAGE DataKinds #-}

module Recs.System where

import Recs.Types
import Effectful
import Effectful.State.Static.Local

runSystem :: Eff (State (SystemState m) : es) a -> Eff es (a, SystemState m)
runSystem = runState (MkSystemState undefined undefined)
