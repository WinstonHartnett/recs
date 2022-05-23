{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Recs.System where

import Control.Lens hiding (from)
import Effectful
import Effectful.State.Static.Local
import Recs.Query (In, QueryItems, Queryable, unsafeGetQueryStore, unsafeModifyQueryStore)
import Recs.Types
import Data.Default (Default(def))

runSystem :: Eff (State SystemState : es) a -> Eff es (a, SystemState)
runSystem = runState (MkSystemState def)

-- | Ensure that this system's types are known by the ECS.
registerSystemTypes :: Ecs es => Queryable q => q -> Eff es ()
registerSystemTypes = undefined

nab ::
  forall c q es.
  System es =>
  (In (QueryItems q) (Nab c) ~ 'True, Component c) =>
  QueryHandle q ->
  Eff es (Elem (Layout c))
nab qh =
  unsafeGetQueryStore @c qh >>= \targetStore -> liftIO do
    storageLookup targetStore (qh ^. #entity . _1) (qh ^. #entity . _2)

stow ::
  forall c q es.
  System es =>
  (In (QueryItems q) (Stow c) ~ 'True, Component c) =>
  QueryHandle q ->
  (Elem (Layout c)) ->
  Eff es ()
stow qh c =
  unsafeModifyQueryStore @c qh \s -> liftIO do
    storageModify s (qh ^. #entity . _1) (qh ^. #entity . _2) c
