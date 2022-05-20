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

module Recs.Core (TypeId (..), EntityId (..), ArchId (..), invalidArchId, Identify (..), Storage (..), Component (..)) where

import Data.Bits
import Data.Coerce
import Data.Hashable
import Data.Primitive.PVar
import Data.Vector.Unboxed.Deriving
import Data.Word

import GHC.Generics (Generic)

import Data.Either (fromRight)
import Data.HashMap.Internal (Hash)
import Data.Kind (Type)
import Data.Typeable (Proxy (Proxy), Typeable, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint (..))
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
  --   TODO Generate these w/ TH
  identify = case typeRepFingerprint . typeRep $ Proxy @t of
    f@(Fingerprint h _) -> (fromRight (error "Word size mismatch") $ tryFrom h, f)

instance Typeable t => Identify t

class Storage a where
  type Elem a

  -- | Add a new row with a given entity.
  storageInsert :: a -> EntityId -> Elem a -> IO a

  -- | Remove a row at the given index.
  --   This performs a swap-remove on the store-level with the last element.
  --
  --   __Safety:__ the corresponding archetype entity entry must be immediately updated
  --   to reflect the swap-remove.
  storageRemove :: a -> EntityId -> Int -> IO a

  -- | Lookup the row at the given index.
  storageLookup :: a -> EntityId -> Int -> IO (Elem a)

  -- | Write a new element to the given index.
  storageModify :: a -> EntityId -> Int -> Elem a -> IO a

  -- | Instantiate a collection of the given type.
  storageInit :: IO a

{- | 'Component' specifies which storage structure to use for each type.

   Storage defaults to 'Unboxed' for performance reasons. You must derive
   'Component via Boxed' if you want Boxed storage.
-}
class (Storage (Layout c), Identify c) => Component (c :: Type) where
  type Layout c
