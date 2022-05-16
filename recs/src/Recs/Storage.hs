{-# LANGUAGE AllowAmbiguousTypes #-}

module Recs.Storage where

import Control.Monad.Catch (MonadThrow)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust)
import Data.Primitive.PVar
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed qualified as VU
import GHC.Base (Type)
import Recs.Core
import Recs.Utils

class Storage m a where
  type Elem a

  -- | Add a new row with a given entity.
  storageInsert :: a -> EntityId -> Elem a -> m a

  -- | Remove a row at the given index.
  --   This performs a swap-remove on the store-level with the last element.
  --
  --   __Safety:__ the corresponding archetype entity entry must be immediately updated
  --   to reflect the swap-remove.
  storageRemove :: a -> EntityId -> Int -> m a

  -- | Lookup the row at the given index.
  storageLookup :: a -> EntityId -> Int -> m (Elem a)

  -- | Write a new element to the given index.
  storageModify :: a -> EntityId -> Int -> Elem a -> m a

  -- | Instantiate a collection of the given type.
  storageInit :: m a

{- | 'Component' specifies which storage structure to use for each type.

   Storage defaults to 'Unboxed' for performance reasons. You must derive
   'Component via Boxed' if you want Boxed storage.
-}
class (Monad m, Storage m (Layout c), Identify c) => Component m (c :: Type) where
  type Layout c

instance (MonadPrim RealWorld m, MonadThrow m) => Storage m (GIOVector c) where
  type Elem (GIOVector c) = c

  storageInsert v _ e = VR.push v e >> pure v

  storageRemove v _ idx =
    -- Moves the last element to fill the gap left by the 'idx' removal
    VR.pop v >>= maybe (pure v) \e -> VR.write v idx e >> pure v

  storageLookup v _ idx = VR.read v idx

  storageModify v _ idx e = VR.write v idx e >> pure v

  storageInit = VR.new

instance (MonadPrim RealWorld m, MonadThrow m, VU.Unbox c) => Storage m (GUIOVector c) where
  type Elem (GUIOVector c) = c

  storageInsert v _ e = VR.push v e >> pure v

  storageRemove v _ idx = do
    VR.pop v >>= maybe (pure v) \e -> VR.write v idx e >> pure v

  storageLookup v _ idx = VR.read v idx

  storageModify v _ idx e = VR.write v idx e >> pure v

  storageInit = VR.new

instance Monad m => Storage m (HM.HashMap EntityId c) where
  type Elem (HM.HashMap EntityId c) = c

  storageInsert h eId e = pure $ HM.insert eId e h

  storageRemove h eId _ = pure $ HM.delete eId h

  storageLookup h eId _ = pure . fromJust $ HM.lookup eId h

  storageModify h eId _ e = pure $ HM.insert eId e h

  storageInit = pure $ HM.empty
