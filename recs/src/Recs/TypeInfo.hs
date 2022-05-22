{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.TypeInfo where

import Data.HashMap.Internal qualified as HMS
import Data.Primitive.PVar (atomicModifyIntPVar, readPVar)
import Data.Vector qualified as V
import Effectful
import Effectful.State.Static.Local (get, modify)
import Recs.Types
import Recs.Utils

pendingTypeId :: Ecs es => Eff es TypeId
pendingTypeId = do
  ecs <- get @World
  from <$> readPVar ecs.typeInfo.nextTypeId

reserveTypeId :: Ecs es => Eff es TypeId
reserveTypeId = do
  ecs <- get @World
  atomicModifyIntPVar ecs.typeInfo.nextTypeId \tId -> (tId + 1, from tId)

{- | Retrieve this type's ID from the global type registry, adding any necessary
   information to the type registry.
-}
identified :: forall c es. (Component c, Ecs es) => Eff es TypeId
identified = do
  ecs <- get @World
  case uncurry HMS.lookup' (identify @c) ecs.typeInfo.types of
    Just tId -> pure tId
    Nothing -> do
      let (h, f) = identify @c
      nextTId <- reserveTypeId
      let updateTInfo =
            let pushStorageDict = (`V.snoc` toSomeStorageDict (mkStorageDict @(Layout c)))
                pushNewType = HMS.insert' h f nextTId
             in over #typeInfo (over #storageDicts pushStorageDict . over #types pushNewType)
      modify @World updateTInfo
      pure nextTId
