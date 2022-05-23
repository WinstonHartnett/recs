{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.EntityInfo (
  EntityRecord,
  EntityInfo,
  reserveEntityId,
  freeEntityId,
  verifyFlushedEntities,
  getEntityMeta
) where

import Control.Monad (forM_, unless)
import Data.Primitive.PVar (atomicReadIntPVar, atomicSubIntPVar, modifyPVar_, readPVar, writePVar)
import Data.Vector.Growable qualified as VR
import Effectful
import Recs.Utils
import Recs.Types
import Effectful.State.Static.Local (get)

getEntityMeta :: Ecs es => EntityId -> Eff es EntityMeta
getEntityMeta eId = do
  ecs <- get @World
  VR.read ecs.entityInfo.records (from eId)

getArchFromEntityId :: Ecs es => EntityId -> Eff es Archetype
getArchFromEntityId eId = do
  ecs <- get @World
  meta <- getEntityMeta eId
  VR.read ecs.archetypes.archetypes $ from meta.location.archId

{- | Concurrently reserve a new 'EntityId'.

   __Safety:__ only safe while not flushing entity IDs.
-}

reserveEntityId :: Ecs es => Eff es EntityId
reserveEntityId = do
  ecs <- get @World
  n <- atomicSubIntPVar ecs.entityInfo.freeCursor 1
  if n > 0
    then VR.read ecs.entityInfo.pending (n - 1)
    else from . subtract n <$> VR.length ecs.entityInfo.records

{- | Free an 'EntityId'.

   __Safety:__ not thread-safe.
-}
freeEntityId :: Ecs es => EntityId -> Eff es ()
freeEntityId eId = do
  verifyFlushedEntities
  ecs <- get @World
  VR.push ecs.entityInfo.pending eId
  writePVar ecs.entityInfo.freeCursor =<< VR.length ecs.entityInfo.pending

-- | Verify that all pending entity IDs have been committed.
verifyFlushedEntities :: Ecs es => Eff es ()
verifyFlushedEntities = do
  ecs <- get @World
  freeCursor <- atomicReadIntPVar $ ecs.entityInfo.freeCursor
  pendingLen <- VR.length $ ecs.entityInfo.pending
  unless (freeCursor == pendingLen) (error "This operation requires flushed entities!")

{- | Flush pending entity IDs from the queue.
   Registers new 'EntityRecord's for each pending 'EntityId' using the supplied
   'recordCommit' function (which usually allocates into the empty archetype).

   __Safety__: not thread-safe.
-}
flushEntities ::
  Ecs es =>
  (EntityId -> EntityRecord -> Eff es EntityRecord) ->
  Eff es ()
flushEntities recordCommit = do
  ecs <- get @World
  let updateEntries lst = forM_ @[] lst \idx -> do
        entry <- VR.read ecs.entityInfo.records idx
        newRecord <-
          recordCommit
            ( MkEntityId
                { generation = entry.generation
                , ident = tryFrom' idx
                }
            )
            entry.location
        VR.write
          ecs.entityInfo.records
          idx
          ( MkEntityMeta
              { generation = entry.generation
              , location = newRecord
              }
          )
  newFreeCursor <- do
    currFreeCursor <- readPVar ecs.entityInfo.freeCursor
    if currFreeCursor >= 0
      then pure currFreeCursor
      else do
        -- Allocate new IDs in the entity meta vector.
        oldRecordsLen <- VR.length ecs.entityInfo.records
        modifyPVar_ ecs.entityInfo.len (+ (-currFreeCursor))
        updateEntries [negate currFreeCursor .. oldRecordsLen + negate currFreeCursor]
        writePVar ecs.entityInfo.freeCursor 0
        pure 0
  pendingLen <- VR.length ecs.entityInfo.pending
  modifyPVar_ ecs.entityInfo.len (+ (pendingLen - newFreeCursor))
  updateEntries [newFreeCursor .. pendingLen]
