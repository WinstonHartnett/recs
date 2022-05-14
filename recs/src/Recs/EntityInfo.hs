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
) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader

import Data.Default
import Data.Primitive.PVar
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed.Deriving
import Data.Word

import GHC.Generics (Generic)

import Recs.Core
import Recs.Utils

import Witch hiding (over)

data EntityRecord = MkEntityRecord
  { archId :: !ArchId
  , idx :: !Int
  }
  deriving (Generic, Show)

derivingUnbox
  "EntityRecord"
  [t|EntityRecord -> (ArchId, Int)|]
  [|
    \(MkEntityRecord a i) ->
      (a, i)
    |]
  [|uncurry MkEntityRecord|]

data EntityMeta = MkEntityMeta
  { generation :: !Word32
  , location :: !EntityRecord
  }
  deriving (Generic)

derivingUnbox
  "EntityMeta"
  [t|EntityMeta -> (Word32, EntityRecord)|]
  [|\(MkEntityMeta g l) -> (g, l)|]
  [|uncurry MkEntityMeta|]

-- | Records information about Entities stored in the ECS.
data EntityInfo = MkEntityInfo
  { records :: GUIOVector EntityMeta
  -- ^ 'records' is indexed by an EntityId
  , pending :: GUIOVector EntityId
  , freeCursor :: IOPVar Int
  , len :: IOPVar Int
  }
  deriving (Generic)

instance {-# OVERLAPPING #-} Default (IO EntityInfo) where
  def = do
    records <- VR.new
    pending <- VR.new
    freeCursor <- newPVar 0
    len <- newPVar 0
    pure $
      MkEntityInfo
        { records = records
        , pending = pending
        , freeCursor = freeCursor
        , len = len
        }

{- | Concurrently reserve a new 'EntityId'.

   __Safety:__ only safe while not flushing entity IDs.
-}
reserveEntityId :: (MonadThrow m, MonadPrim RealWorld m) => EntityInfo -> m EntityId
reserveEntityId eInfo = do
  n <- atomicSubIntPVar eInfo.freeCursor 1
  if n > 0
    then VR.read eInfo.pending (n - 1)
    else from . subtract n <$> VR.length eInfo.records

{- | Free an 'EntityId'.

   __Safety:__ not thread-safe.
-}
freeEntityId :: MonadPrim RealWorld m => EntityInfo -> EntityId -> m ()
freeEntityId eInfo eId = do
  verifyFlushedEntities eInfo
  let pending = eInfo.pending
  VR.push pending eId
  writePVar eInfo.freeCursor =<< VR.length pending

-- | Verify that all pending entity IDs have been committed.
verifyFlushedEntities :: MonadPrim RealWorld m => EntityInfo -> m ()
verifyFlushedEntities eInfo = do
  freeCursor <- atomicReadIntPVar $ eInfo.freeCursor
  pendingLen <- VR.length $ eInfo.pending
  unless (freeCursor == pendingLen) (error "This operation requires flushed entities!")

{- | Flush pending entity IDs from the queue.
   Registers new 'EntityRecord's for each pending 'EntityId' using the supplied
   'recordCommit' function (which usually allocates into the empty archetype).

   __Safety__: not thread-safe.
-}
flushEntities ::
  (MonadPrim RealWorld m, MonadThrow m) =>
  EntityInfo ->
  (EntityId -> EntityRecord -> m EntityRecord) ->
  m ()
flushEntities eInfo recordCommit = do
  let updateEntries lst = forM_ @[] lst \idx -> do
        entry <- VR.read eInfo.records idx
        newRecord <-
          recordCommit
            ( MkEntityId
                { generation = entry.generation
                , ident = tryFrom' idx
                }
            )
            entry.location
        VR.write
          eInfo.records
          idx
          ( MkEntityMeta
              { generation = entry.generation
              , location = newRecord
              }
          )
  newFreeCursor <- do
    currFreeCursor <- readPVar eInfo.freeCursor
    if currFreeCursor >= 0
      then pure currFreeCursor
      else do
        -- Allocate new IDs in the entity meta vector.
        oldRecordsLen <- VR.length eInfo.records
        modifyPVar_ eInfo.len (+ (-currFreeCursor))
        updateEntries [negate currFreeCursor .. oldRecordsLen + negate currFreeCursor]
        writePVar eInfo.freeCursor 0
        pure 0
  pendingLen <- VR.length eInfo.pending
  modifyPVar_ eInfo.len (+ (pendingLen - newFreeCursor))
  updateEntries [newFreeCursor .. pendingLen]
