{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Recs.System where

import Control.Lens hiding (from)
import Recs.Types
import Effectful
import Effectful.State.Static.Local
import Recs.Query (Queryable, QueryItems, In, Nab, QueryHandle, unsafeGetQueryStore, Stow)
import Recs.Archetype (unsafeGetStore)

runSystem :: Eff (State (SystemState m) : es) a -> Eff es (a, SystemState m)
runSystem = runState (MkSystemState undefined undefined)

registerSystem :: Ecs es => Queryable q => q -> Eff es ()
registerSystem = undefined

nab :: forall c q es. Ecs es =>
    (In (QueryItems q) (Nab c) ~ 'True, Component c)
    => QueryHandle q
    -> Eff es (Elem (Layout c))
nab qh = do
  targetStore <- unsafeGetQueryStore @c qh
  liftIO $ storageLookup targetStore (qh ^. #entity . _1) (qh ^. #entity . _2)

stow :: forall c q es. Ecs es =>
     (In (QueryItems q) (Stow c) ~ 'True, Component c)
     => QueryHandle q
     -> (Elem (Layout c))
     -> Eff es ()
stow qh c = do
  targetStore <- unsafeGetQueryStore @c qh
  liftIO $ storageModify targetStore (qh ^. #entity . _1) (qh ^. #entity . _2) c
  pure ()
