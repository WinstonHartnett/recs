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

import Control.Applicative (liftA2, (<|>))
import Control.Lens hiding (from)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Maybe

import Data.Coerce
import Data.Default
import Data.Either (fromRight)
import Data.Generics.Labels
import Data.HashMap.Internal (Hash)
import Data.HashMap.Internal qualified as HM
import Data.HashMap.Strict qualified as HMS
import Data.Hashable
import Data.IORef
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Kind
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Primitive.PVar
import Data.Proxy (Proxy (..))
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Heap qualified as V
import Data.Vector.Algorithms.Search qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Growable qualified as VR
import Data.Vector.Mutable qualified as VM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word

import GHC.Base (Any, IO (..), RealWorld)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeLits

import Recs.Core
import Recs.Utils

import Unsafe.Coerce

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
            (entry ^. #location)
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
