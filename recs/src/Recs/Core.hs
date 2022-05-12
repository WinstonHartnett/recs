{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.Core (TypeId (..), EntityId (..), ArchId (..)) where

import Control.Applicative (liftA2, (<|>))
import Control.Lens hiding (from)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.Bits
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

import Recs.Utils

import Unsafe.Coerce

import Witch hiding (over)

-- | Unique ID of a type known by the ECS.
newtype TypeId = MkTypeId Word16
  deriving (Generic, Eq, Ord, Show, Prim)

derivingUnbox "TypeId" [t|TypeId -> Word16|] [|\(MkTypeId w) -> w|] [|MkTypeId|]

instance From TypeId Int where
  from (MkTypeId w) = from w

instance TryFrom Int TypeId where
  tryFrom i = coerce (MkTypeId <$> tryFrom i)

instance Hashable TypeId

-- | Unique ID of an Archetype.
newtype ArchId = MkArchId Int
  deriving (Generic, Eq, Ord, Show, Prim)

derivingUnbox "ArchId" [t|ArchId -> Int|] [|\(MkArchId i) -> i|] [|MkArchId|]

instance From ArchId Int

instance From Int ArchId

instance Hashable ArchId

invalidArchId :: ArchId
invalidArchId = MkArchId (-1)

-- | Unique ID of an Entity.
data EntityId = MkEntityId
  { generation :: {-# UNPACK #-} !Word32
  , ident :: {-# UNPACK #-} !Word32
  }
  deriving (Generic, Eq, Ord, Show)

derivingUnbox
  "EntityId"
  [t|EntityId -> (Word32, Word32)|]
  [|\(MkEntityId g i) -> (g, i)|]
  [|uncurry MkEntityId|]

instance From EntityId Int where
  from (MkEntityId{generation, ident}) =
    let generation' = from @Word32 @Word64 generation
        ident' = from @Word32 @Word64 ident
     in fromIntegral $ (generation' `shiftL` 31) .|. ident'

instance From Int EntityId where
  from i =
    let i' = fromIntegral @Int @Word64 i
     in MkEntityId
          { generation = fromIntegral $ i' `shiftR` 31
          , ident =
              fromIntegral $
                i' .&. complement (fromIntegral @Int @Word64 $ 1 `shiftR` 31)
          }

instance Hashable EntityId

-- | Provide a unique hash for each type.
class Typeable t => Identify t where
  identify :: (Hash, Fingerprint)
  default identify :: (Hash, Fingerprint)
  --   TODO Generate these w/ TH
  identify = case typeRepFingerprint . typeRep $ Proxy @t of
    f@(Fingerprint h _) -> (fromRight (error "Word size mismatch") (tryFrom h), f)

instance {-# OVERLAPPABLE #-} Typeable t => Identify t

class Storage m a where
  type Elem a

  -- | Add a new row with a given entity.
  storageInsert :: a -> EntityId -> Elem a -> m ()

  -- | Remove a row at the given index.
  --   This performs a swap-remove on the store-level with the last element.
  --
  --   __Safety:__ the corresponding archetype entity entry must be immediately updated
  --   to reflect the swap-remove.
  storageRemove :: a -> EntityId -> Int -> m ()

  -- | Lookup the row at the given index.
  storageLookup :: a -> EntityId -> Int -> m (Elem a)

  -- | Write a new element to the given index.
  storageModify :: a -> EntityId -> Int -> Elem a -> m ()

  -- | Instantiate a collection of the given type.
  storageInit :: m a

instance MonadIO m => Storage m (IORef (HM.HashMap EntityId v)) where
  type Elem (IORef (HM.HashMap EntityId v)) = v

  storageInsert i eId e = liftIO $ modifyIORef i (\h -> HM.insert eId e h)

  -- storageInsert

instance (MonadPrim RealWorld m, MonadThrow m) => Storage m (GIOVector c) where
  type Elem (GIOVector c) = c

  storageInsert v eId e = VR.push v e

  storageRemove v eId idx = do
    -- Moves the last element to fill the gap left by the 'idx' removal
    n <- VR.pop v
    case n of
      Just p -> VR.write v idx p
      Nothing -> pure ()

  storageLookup v eId idx = VR.read v idx

  storageModify v eId idx = VR.write v idx

  storageInit = VR.new

instance (MonadPrim RealWorld m, MonadThrow m, VU.Unbox c) => Storage m (GUIOVector c) where
  type Elem (GUIOVector c) = c

  storageInsert v eId e = VR.push v e

  storageRemove v eId idx = do
    n <- VR.pop v
    case n of
      Just p -> VR.write v idx p
      Nothing -> pure ()

  storageLookup v eId idx = VR.read v idx

  storageModify v eId idx = VR.write v idx

  storageInit = VR.new
