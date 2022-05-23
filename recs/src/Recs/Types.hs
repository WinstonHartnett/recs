{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}

module Recs.Types where

import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Default (Default (def))
import Data.Either (fromRight)
import Data.Generics.Labels ()
import Data.HashMap.Internal qualified as HM
import Data.HashMap.Strict qualified as HMS
import Data.Hashable (Hashable)
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Primitive.PVar (newPVar)
import Data.Sequence qualified as SQ
import Data.Typeable (Proxy (..), Typeable, typeRep, typeRepFingerprint)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word32, Word64)
import Effectful
import Effectful.Prim (Prim)
import Effectful.State.Static.Local
import GHC.Base (Any, liftM)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Generics (Generic)
import Recs.Utils
import Unsafe.Coerce (unsafeCoerce)

newtype Globals = MkGlobals {unGlobals :: GIOVector Any}
  deriving (Generic)

instance {-# OVERLAPPING #-} Default (IO Globals) where
  def = MkGlobals <$> VR.new

----------------------------------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------------------------------

-- | Unique ID of a type known by the ECS.
newtype TypeId = MkTypeId Int
  deriving (Generic, Eq, Ord, Show, Hashable)

derivingUnbox "TypeId" [t|TypeId -> Int|] [|\(MkTypeId w) -> w|] [|MkTypeId|]

instance From TypeId Int

instance From Int TypeId

invalidTypeId :: TypeId
invalidTypeId = from @Int (-1)

-- TODO This sucks o-wise
typeIdDiff :: VU.Vector TypeId -> VU.Vector TypeId -> VU.Vector TypeId
typeIdDiff a b =
  let buildISet = IS.fromDistinctAscList . VG.toList . VG.map from
      a' = buildISet a
      b' = buildISet b
   in VG.map (MkTypeId . fromIntegral) . VG.fromList . IS.toList $ intSetDiff a' b'

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
  storageInsert :: a -> EntityId -> Elem a -> IO ()

  -- | Remove a row at the given index.
  --   This performs a swap-remove on the store-level with the last element.
  --
  --   __Safety:__ the corresponding archetype entity entry must be immediately updated
  --   to reflect the swap-remove.
  storageRemove :: a -> EntityId -> Int -> IO ()

  -- | Lookup the row at the given index.
  storageLookup :: a -> EntityId -> Int -> IO (Elem a)

  -- | Write a new element to the given index.
  storageModify :: a -> EntityId -> Int -> Elem a -> IO ()

  -- | Instantiate a collection of the given type.
  storageInit :: IO a

{- | 'Component' specifies which storage structure to use for each type.

   Storage defaults to 'Unboxed' for performance reasons. You must derive
   'Component via Boxed' if you want Boxed storage.
-}
class (Storage (Layout c), Elem (Layout c) ~ c, Identify c) => Component (c :: Type) where
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

instance {-# OVERLAPPING #-} Default (IO EntityInfo) where
  def = do
    records <- VR.new
    pending <- VR.new
    freeCursor <- newPVar 0
    len <- newPVar 0
    pure
      MkEntityInfo
        { records = records
        , pending = pending
        , freeCursor = freeCursor
        , len = len
        }

----------------------------------------------------------------------------------------------------
-- Type Information
----------------------------------------------------------------------------------------------------

data StorageDict s = MkStorageDict
  { _storageInsert :: s -> EntityId -> Elem s -> IO ()
  , _storageRemove :: s -> EntityId -> Int -> IO ()
  , _storageLookup :: s -> EntityId -> Int -> IO (Elem s)
  , _storageModify :: s -> EntityId -> Int -> Elem s -> IO ()
  , _storageInit :: !(IO s)
  }
  deriving (Generic)

type SomeStorageDict = StorageDict Any

-- | Convert a 'StorageDict' to an untyped 'SomeStorageDict'.
toSomeStorageDict :: forall s. StorageDict s -> SomeStorageDict
toSomeStorageDict = unsafeCoerce @(StorageDict s) @SomeStorageDict

-- | Unsafely coerce an unqualified 'SomeStorageDict' to a 'StorageDict'.
unsafeToStorageDict :: forall a. Storage a => SomeStorageDict -> StorageDict a
unsafeToStorageDict = unsafeCoerce

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

instance {-# OVERLAPPING #-} Default (IO TypeInfo) where
  def = do
    nextTypeId <- newPVar 0
    pure
      MkTypeInfo
        { nextTypeId = nextTypeId
        , types = HMS.empty
        , storageDicts = V.empty
        }

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
  { archId :: !ArchId
  , components :: {-# UNPACK #-} !(Vector Any)
  -- ^ Untyped component stores.
  , entities :: {-# UNPACK #-} !(GUIOVector EntityId)
  -- ^ Entities (in order of storage).
  , types :: {-# UNPACK #-} !(UVector TypeId)
  -- ^ Type of components stored in this 'Archetype'.
  , typeMap :: {-# UNPACK #-} !(GUIOVector Int)
  -- ^ Vector indexed by 'TypeId' pointing to the relevant store's index.
  , edges :: {-# UNPACK #-} !(GUIOVector Edge)
  -- ^ Edges in the 'Archetype' graph.
  }
  deriving (Generic)

data Archetypes = MkArchetypes
  { archetypes :: {-# UNPACK #-} !(GIOVector Archetype)
  , graph :: GIOVector (GUIOVector ArchId)
  }
  deriving (Generic)

instance {-# OVERLAPPING #-} Default (IO Archetypes) where
  def = do
    archetypes <- VR.new
    graph <- VR.thaw =<< liftM (VG.singleton) (VR.thaw [MkArchId 0])
    pure
      MkArchetypes
        { archetypes = archetypes
        , graph = graph
        }

data ArchetypeSearch = Add !(VU.Vector TypeId) | Remove !(VU.Vector TypeId)

----------------------------------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------------------------------

data ArchetypeMove
  = MovedArchetype
      { src :: {-# UNPACK #-} !EntityRecord
      , dest :: {-# UNPACK #-} !EntityRecord
      }
  | SameArchetype {-# UNPACK #-} !EntityRecord

data Command
  = MkTag
      { fingerprint :: !Fingerprint
      , commit :: !(ArchetypeMove -> Ecs' ())
      }
  | MkUntag
      { fingerprint :: !Fingerprint
      , commit :: !(ArchetypeMove -> Ecs' ())
      }
  | MkDespawn

data EntityCommands = MkEntityCommands
  { entityId :: !EntityId
  , queue :: SQ.Seq Command
  }
  deriving Generic

type CommandBuilderE es = State EntityCommands

newtype Commands = MkCommands {unCommands :: SQ.Seq EntityCommands}
  deriving (Generic)

instance Default Commands where
  def =
    MkCommands
      { unCommands = SQ.empty
      }

----------------------------------------------------------------------------------------------------
-- Queries
----------------------------------------------------------------------------------------------------

{- Query Operators
-}
data Nab c

data Stow c

data Tag c

data With c

data Not c

type Without c = Not (With c)

data a |&| b

infixr 4 |||

data a ||| b

{- Reified Query Data
-}

-- | Subject of a query like 'OpNab'.
data QuerySubject
  = -- | Component is optional. Query will not fail if missing.
    SubMaybe !TypeId
  | -- | Component is mandatory. Query will fail if missing.
    SubBase !TypeId
  deriving (Generic, Eq, Show)

instance Hashable QuerySubject

-- TODO Allow bundled subjects
data QueryOperator
  = -- | Archetype has this component for read access.
    OpNab !QuerySubject
  | -- | Archetype has this component for write access.
    OpStow !QuerySubject
  | -- | Archetype must have this component.
    OpWith !TypeId
  | -- | Archetype must not satisfy this query.
    OpNot !QueryOperator
  | -- | Archetype must satisfy both queries.
    OpAnd !QueryOperator !QueryOperator
  | -- | Archetype must satisfy either query.
    OpOr !QueryOperator !QueryOperator
  deriving (Generic, Eq, Show)

instance Hashable QueryOperator

-- | State of a query iterator.
data QueryTraversal = MkQueryTraversal
  { currArchId :: !ArchId
  , currArch :: !Archetype
  }
  deriving (Generic)

newtype SomeQuery = MkSomeQuery
  { unSomeQuery :: V.Vector QueryTraversal
  }
  deriving (Generic)

-- | A query iterator.
data QueryHandle q = MkQueryHandle
  { entity :: (EntityId, Int)
  -- ^ Index of current entity and its ID.
  , trav :: QueryTraversal
  -- ^ Points to information about the currently iterated archetype.
  }
  deriving (Generic)

newtype Query q = MkQuery {unQuery :: V.Vector (QueryHandle q)}

newtype Global c = MkGlobal
  { unGlobal :: c
  }

newtype Local c = MkLocal
  { unLocal :: c
  }

data QueryInfo = MkQueryInfo
  { cachedQueries :: HMS.HashMap QueryOperator SomeQuery
  }
  deriving (Generic)

instance {-# OVERLAPPING #-} Default QueryInfo where
  def = MkQueryInfo{cachedQueries = HMS.empty}

----------------------------------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------------------------------

-- | System-local state.
data SystemState = MkSystemState
  { commands :: !Commands
  }
  deriving Generic

type System es = (State SystemState :> es, Ecs es)

data World = MkWorld
  { archetypes :: !Archetypes
  , globals :: !Globals
  , entityInfo :: !EntityInfo
  , typeInfo :: !TypeInfo
  , queryInfo :: !QueryInfo
  }
  deriving (Generic)

type Ecs es = '[State World, Prim, IOE] :>> es

type Ecs' = Eff '[State World, Prim, IOE]

instance {-# OVERLAPPING #-} Default (IO World) where
  def = do
    archetypes <- def
    globals <- def
    entityInfo <- def
    typeInfo <- def
    queryInfo <- def
    pure
      MkWorld
        { archetypes = archetypes
        , globals = globals
        , entityInfo = entityInfo
        , typeInfo = typeInfo
        , queryInfo = queryInfo
        }
