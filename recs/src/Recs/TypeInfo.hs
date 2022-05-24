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
import Data.Hashable (Hashable (..))
import Data.Maybe (fromMaybe)
import Data.Primitive.PVar (atomicModifyIntPVar, readPVar)
import Data.Vector qualified as V
import Effectful
import Effectful.State.Static.Local (get, modify)
import GHC.Fingerprint (Fingerprint (Fingerprint))
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
identified = identifiedByFingerprint (snd $ identify @c) . toSomeStorageDict $ mkStorageDict @(Layout c)

identifiedByFingerprint :: Ecs es => Fingerprint -> SomeStorageDict -> Eff es TypeId
identifiedByFingerprint f@(Fingerprint h _) sd = do
  ecs <- get @World
  case HMS.lookup' (tryFrom' h) f ecs.typeInfo.types of
    Just tId -> pure tId
    Nothing -> do
      nextTId <- reserveTypeId
      let updateTInfo =
            let pushStorageDict = (`V.snoc` sd)
                pushNewType = HMS.insert' (tryFrom' h) f nextTId
             in over #typeInfo (over #storageDicts pushStorageDict . over #types pushNewType)
      modify @World updateTInfo
      pure nextTId

identifiedByFingerprint' :: Ecs es => Fingerprint -> Eff es (Maybe TypeId)
identifiedByFingerprint' f@(Fingerprint h _) = do
  ecs <- get @World
  pure $ HMS.lookup' (tryFrom' h) f ecs.typeInfo.types
