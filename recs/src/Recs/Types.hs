{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}

module Recs.Types where

import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Generics.Labels ()
import Data.HashMap.Internal qualified as HM
import Data.HashMap.Strict qualified as HMS
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence qualified as SQ
import Data.Typeable (Proxy (..), Typeable, typeRep, typeRepFingerprint)
import Data.Vector qualified as V
import Data.Vector.Growable qualified as VR
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Deriving
import Data.Word (Word16, Word32, Word64)
import Effectful
import Effectful.Prim (Prim)
import Effectful.State.Static.Local
import GHC.Base (Any)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Generics (Generic)
import Recs.Utils
import Unsafe.Coerce (unsafeCoerce)
import Witch (From (..), TryFrom (tryFrom))
import qualified Data.IntMap.Strict as IM

newtype Globals = MkGlobals {unGlobals :: GIOVector Any}
  deriving (Generic)

----------------------------------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------------------------------

-- | Unique ID of a type known by the ECS.
newtype TypeId = MkTypeId Int
  deriving (Generic, Eq, Ord, Show)

derivingUnbox "TypeId" [t|TypeId -> Int|] [|\(MkTypeId w) -> w|] [|MkTypeId|]

invalidTypeId :: TypeId
invalidTypeId = from @Int (-1)

instance From TypeId Int where
  -- from (MkTypeId w) = from w

instance From Int TypeId where
  -- tryFrom i = coerce (MkTypeId <$> tryFrom i)

instance Hashable TypeId

-- | Unique ID of an Archetype.
newtype ArchId = MkArchId Int
  deriving (Generic, Eq, Ord, Show, Hashable)

derivingUnbox "ArchId" [t|ArchId -> Int|] [|\(MkArchId i) -> i|] [|MkArchId|]

instance From ArchId Int

instance From Int ArchId

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
  identify :: (HM.Hash, Fingerprint)
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

----------------------------------------------------------------------------------------------------
-- Entity Information
----------------------------------------------------------------------------------------------------
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
  { records :: {-# UNPACK #-} !(GUIOVector EntityMeta)
  -- ^ 'records' is indexed by an EntityId
  , pending :: {-# UNPACK #-} !(GUIOVector EntityId)
  , freeCursor :: {-# UNPACK #-} !(IOPVar Int)
  , len :: {-# UNPACK #-} !(IOPVar Int)
  }
  deriving (Generic)

----------------------------------------------------------------------------------------------------
-- Type Information
----------------------------------------------------------------------------------------------------

-- data SomeStorageDict = MkSomeStorageDict
--   { _storageInsert :: Any -> EntityId -> Any -> Any Any
--   , _storageRemove :: Any -> EntityId -> Int -> Any Any
--   , _storageLookup :: Any -> EntityId -> Int -> Any Any
--   , _storageModify :: Any -> EntityId -> Int -> Any -> Any Any
--   , _storageInit :: !(Any Any)
--   }

data StorageDict s = MkStorageDict
  { _storageInsert :: s -> EntityId -> Elem s -> IO s
  , _storageRemove :: s -> EntityId -> Int -> IO s
  , _storageLookup :: s -> EntityId -> Int -> IO (Elem s)
  , _storageModify :: s -> EntityId -> Int -> Elem s -> IO s
  , _storageInit :: !(IO s)
  }
  deriving Generic

type SomeStorageDict = StorageDict Any

toSomeStorageDict :: forall s. StorageDict s -> SomeStorageDict
toSomeStorageDict = unsafeCoerce @(StorageDict s) @SomeStorageDict

-- instance From (StorageDict s) SomeStorageDict where
--   from = unsafeCoerce @(StorageDict s) @SomeStorageDict

-- | Reify a 'Storage' instance.
mkStorageDict :: forall a. Storage a => StorageDict a
mkStorageDict =
  MkStorageDict
    { _storageInsert = storageInsert
    , _storageRemove = storageRemove
    , _storageLookup = storageLookup
    , _storageModify = storageModify
    , _storageInit = storageInit
    }

data TypeInfo = MkTypeInfo
  { nextTypeId :: {-# UNPACK #-} !(IOPVar Int)
  -- ^ Can only be accessed atomically.
  , types :: !(HMS.HashMap Fingerprint TypeId)
  , storageDicts :: !(V.Vector SomeStorageDict)
  }
  deriving (Generic)

----------------------------------------------------------------------------------------------------
-- Archetypes
----------------------------------------------------------------------------------------------------

-- | An edge in the Archetype graph.
data Edge = MkEdge
  { add :: !(Maybe ArchId)
  , remove :: !(Maybe ArchId)
  }
  deriving (Generic, Show)

derivingUnbox
  "Edge"
  [t|Edge -> (ArchId, ArchId)|]
  [|
    \e ->
      (fromMaybe invalidArchId e.add, fromMaybe invalidArchId e.remove)
    |]
  [|
    \(add, remove) ->
      let convertArchId i
            | invalidArchId == i = Nothing
            | otherwise = Just i
       in MkEdge (convertArchId add) (convertArchId remove)
    |]

data Archetype = MkArch
  { components :: {-# UNPACK #-} !(Vector Any)
  -- ^ Untyped component stores.
  , entities :: {-# UNPACK #-} !(GUIOVector EntityId)
  -- ^ Entities (in order of storage).
  , types :: {-# UNPACK #-} !(UVector TypeId)
  -- ^ Type of components stored in this 'Archetype'.
  , typeMap :: {-# UNPACK #-} !(GUIOVector Int)
  -- ^ Vector indexed by 'TypeId' pointing to the relevant store's index.
  , edges :: {-# UNPACK #-} !(GUIOVector Edge)
  }
  deriving (Generic)

data Archetypes = MkArchetypes
  { archetypes  :: {-# UNPACK #-} !(GIOVector Archetype)
  }
  deriving (Generic)

----------------------------------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------------------------------

data ArchetypeMove
  = MovedArchetype
      { src :: {-# UNPACK #-} !EntityRecord
      , dest :: {-# UNPACK #-} !EntityRecord
      }
  | SameArchetype {-# UNPACK #-} !EntityRecord

data Command es
  = MkTag
      { fingerprint :: !Fingerprint
      , commit :: !(World -> Eff es ())
      }
  | MkUntag
      { fingerprint :: !Fingerprint
      , commit :: !(World -> Eff es ())
      }
  | Despawn

data EntityCommands es = MkEntityCommands
  { entityId :: !EntityId
  , queue :: SQ.Seq (Command es)
  }

type CommandBuilder es = State (EntityCommands es)

newtype Commands es = MkCommands {unCommands :: SQ.Seq (EntityCommands es)}

----------------------------------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------------------------------

-- | System-local state.
data SystemState es = MkSystemState
  { commands :: !(Commands es)
  , world :: !World
  }

type System es = (State (SystemState es) :> es, Ecs es)

data World = MkWorld
  { archetypes :: Archetypes
  , globals :: Globals
  , entityInfo :: EntityInfo
  , typeInfo :: TypeInfo
  }
  deriving (Generic)

type Ecs es = (State World :> es, Prim :> es, IOE :> es)
