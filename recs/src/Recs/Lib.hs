
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


module Recs.Lib where

import           Control.Applicative           ((<|>),liftA2)
import           Control.Lens                  hiding (from)
import           Control.Monad.Catch           (MonadCatch,MonadMask,MonadThrow)
import           Control.Monad.Primitive       (PrimMonad(..))
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe

import qualified Data.Sequence as SQ
import           Data.Coerce
import           Data.Default
import           Data.Either                   (fromRight)
import           Data.Generics.Labels
import qualified Data.HashMap.Internal         as HM
import           Data.HashMap.Internal         (Hash)
import qualified Data.HashMap.Strict           as HMS
import           Data.Hashable
import           Data.IORef
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Kind
import           Data.Maybe                    (fromJust,fromMaybe,isJust)
import           Data.Primitive.PVar
import           Data.Proxy                    (Proxy(..))
import qualified Data.Sequence                 as SQ
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Data.Typeable                 (Typeable,typeRep,typeRepFingerprint)
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Heap   as V
import qualified Data.Vector.Algorithms.Search as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.Vector.Growable          as VR
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Primitive         as VP
import qualified Data.Vector.Unboxed           as VU
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.Word

import           GHC.Base                      (Any,IO(..),RealWorld)
import           GHC.Fingerprint               (Fingerprint(..))
import           GHC.Generics                  (Generic)
import           GHC.Stack                     (HasCallStack)
import           GHC.TypeLits

import           Unsafe.Coerce

import           Witch                         hiding (over)

tryFrom' :: TryFrom a b => a -> b
tryFrom' = fromRight undefined . tryFrom

untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM p f a = do
  cond <- a >>= p
  if cond
    then a
    else untilM p f (a >>= f)

instance (VU.Unbox a, Hashable a) => Hashable (VU.Vector a) where
  hashWithSalt i = hashWithSalt i . VG.toList

type GIOVector = VR.GrowableIOVector

type GUIOVector = VR.GrowableUnboxedIOVector

type GPIOVector = VR.GrowablePrimitiveIOVector

type GSIOVector = VR.GrowableStorableIOVector

-----------------------------------------------------------------------------------------
-- Typelevel Programming
-----------------------------------------------------------------------------------------
type family IsTuple t where
  IsTuple (a, b) = True
  IsTuple (a, b, c) = True
  IsTuple (a, b, c, d) = True
  IsTuple _ = False

type family (l :: [Type]) ++  (m :: [Type]) :: [Type] where
  (a : as) ++ b = a : (as ++ b)
  '[] ++ b = b

type family Equals a b where
  Equals a a = True
  Equals _ _ = False

type family If c a b where
  If True a _ = a
  If False _ b = b

type family In l e where
  In (a : as) a = True
  In (a : as) e = In as e
  In '[] e = False

type family TupleToList t where
  TupleToList (a, b) = '[a, b]
  TupleToList (a, b, c) = '[a, b, c]
  TupleToList (a, b, c, d) = '[a, b, c, d]

type family ListToTuple l where
  ListToTuple '[a, b] = (a, b)
  ListToTuple '[a, b, c] = (a, b, c)
  ListToTuple '[a, b, c, d] = (a, b, c, d)
  ListToTuple '[a] = a

  -- TupleToList a = '[a]
type family Map f l where
  Map f (a : as) = f a : Map f as
  Map _ '[] = '[]

type family IsList l where
  IsList (a : _) = True
  IsList '[] = True
  IsList _ = False

type family Or a b where
  Or True False = True
  Or False True = True
  Or True True = True
  Or False False = False

-----------------------------------------------------------------------------------------
-- Core Data
-----------------------------------------------------------------------------------------
newtype TypeId = MkTypeId Word16
  deriving (Generic,Eq,Ord,Show,Prim)

instance From TypeId Int where
  from (MkTypeId w) = from w

instance TryFrom Int TypeId where
  tryFrom i = coerce (MkTypeId <$> tryFrom i)

instance Hashable TypeId

newtype ArchId = MkArchId Word32
  deriving (Generic,Eq,Ord,Show,Prim)

invalidArchId :: ArchId
invalidArchId = MkArchId 4_294_967_295

instance From ArchId Int where
  from (MkArchId a) = fromIntegral a

instance TryFrom Int ArchId where
  tryFrom i = coerce (MkArchId <$> tryFrom i)

instance Hashable ArchId

newtype EntityId = MkEntityId Word32
  deriving (Generic,Eq,Ord,Show,Prim)

invalidEntityId :: EntityId
invalidEntityId = MkEntityId 4_294_967_295

instance From EntityId Int where
  from (MkEntityId a) = fromIntegral a

instance TryFrom Int EntityId where
  tryFrom i = coerce (MkEntityId <$> tryFrom i)

instance Hashable EntityId

derivingUnbox "EntityId" [t|EntityId -> Word32|] [|\(MkEntityId i) -> i|] [|MkEntityId|]

derivingUnbox "ArchId" [t|ArchId -> Word32|] [|\(MkArchId i) -> i|] [|MkArchId|]

derivingUnbox "TypeId" [t|TypeId -> Word16|] [|\(MkTypeId w) -> w|] [|MkTypeId|]

-----------------------------------------------------------------------------------------
-- Query Operators
-----------------------------------------------------------------------------------------
data Nab c

data Stow c

data Tag c

data With c

data Not c

type Without c = Not (With c)

data a |&|  b

infixr 4 |||

data a |||  b

-----------------------------------------------------------------------------------------
-- | Subject of a query like 'OpNab'.
data QuerySubject
  = -- | Component is optional. Query will not fail if missing.
    SubMaybe !TypeId
    -- | Component is mandatory. Query will fail if missing.
  | SubBase !TypeId
  deriving (Generic,Eq,Show)

instance Hashable QuerySubject

-- TODO Allow bundled subjects
data QueryOperator
  = -- | Archetype has this component for read access.
    OpNab !QuerySubject
    -- | Archetype has this component for write access.
  | OpStow !QuerySubject
    -- | Archetype must have this component.
  | OpWith !TypeId
    -- | Archetype mustn't satisfy this query.
  | OpNot !QueryOperator
    -- | Archetype must satisfy both queries.
  | OpAnd !QueryOperator !QueryOperator
    -- | Archetype must satisfy either query.
  | OpOr !QueryOperator !QueryOperator
  deriving (Generic,Eq,Show)

instance Hashable QueryOperator

-----------------------------------------------------------------------------------------
-- | State of a query iterator.
data QueryTraversal =
  MkQueryTraversal
  { currArchId :: ArchId
  , currArch   :: Arch
  , matched    :: V.Vector Any
  , matchedOk  :: VU.Vector Bool
  }
  deriving Generic

newtype SomeQuery =
  MkSomeQuery
  { unSomeQuery :: V.Vector QueryTraversal
  }
  deriving Generic

-- | A query iterator.
data QueryHandle q =
  MkQueryHandle
  { -- | Index of current entity and its ID.
    entity   :: (EntityId, Int)
    -- | Points to information about the currently iterated archetype.
  , trav   :: QueryTraversal
  }
  deriving Generic

newtype Query q =
  MkQuery
  { unQuery :: V.Vector (QueryHandle q)
  }
  deriving Generic

newtype Global c =
  MkGlobal
  { unGlobal :: c
  }
  deriving Generic

data Local c =
  MkLocal
  { unLocal :: c
  }
  deriving Generic

-----------------------------------------------------------------------------------------
-- Access Control
-----------------------------------------------------------------------------------------
-- | Optional query wrapper.
data Opt c

type family QueryItems' a b where
  QueryItems' a (b ||| bs) = '[Opt a] ++ QueryItems (b ||| bs)
  QueryItems' a b = '[Opt a] ++ '[Opt (QueryItems b)]

-- | Used to check System-local access permissions, i.e. when using 'nab' or similar
--   'QueryHandle' functions.
type family QueryItems q :: [Type] where
  QueryItems (a |&| b) = QueryItems a ++ QueryItems b
  QueryItems (a ||| b) = QueryItems' (QueryItems a) b
  QueryItems (Nab a) = '[Nab a]
  QueryItems (Stow a) = '[Stow a]
  QueryItems (Tag a) = '[Tag a]
  QueryItems (With a) = '[With a]

-----------------------------------------------------------------------------------------
-- Archetypes
-----------------------------------------------------------------------------------------
data Edge =
  MkEdge
  { add    :: !(Maybe ArchId)
  , remove :: !(Maybe ArchId)
  }
  deriving (Generic,Show)

--------------------------------------------------------------------------------
data ArchSearch
  = Add (VU.Vector TypeId)
  | Remove (VU.Vector TypeId)

data ArchRecord =
  MkArchRecord
  { archId :: !ArchId
  , idx    :: !Int
  }
  deriving (Generic,Show)

-- | = Archetype
--
--   'Arch'etypes store entities with the same components.
--
--   == Archetype Graph
--
--   TODO
data Arch =
  MkArch
  { archId     :: ArchId
    -- | Component storages in this archetype. In the same order as their corresponding
    --   type IDs.
  , components :: V.Vector Any
    -- | Entities contained in this archetype. An entity's index is used to operate on
    --   this archetype's storages.
  , entities   :: GUIOVector EntityId
    -- | Component types corresponding to their relevant 'components' entry.
  , types      :: VU.Vector TypeId
    -- | Edges in the archetype graph.
  , edges      :: GUIOVector Edge
  }
  deriving Generic

derivingUnbox "ArchRecord" [t|ArchRecord -> (ArchId, Int)|] [|\(MkArchRecord a i)
  -> (a, i)|] [|uncurry MkArchRecord|]

-- | = Entity ID Management
--
--   @
--   +-------|------------+----------------+
--   |  new  |    free    |    reserved    |
--   +-------|------------+----------------+
--           |            ^                ^
--           0        freeCursor     pending length@
--
--   == Attribution
--
--   Based on [Bevy's entity ID reservation system](github.com/bevyengine/bevy/blob/main/crates/bevy_ecs/src/entity/mod.rs).
data EntityInfo =
  MkEntityInfo
  { meta       :: GUIOVector ArchRecord
  , pending    :: GUIOVector EntityId
  , freeCursor :: PVar Int RealWorld
  , len        :: PVar Int RealWorld
  }
-- TODO Find out what len is supposed to do?
  deriving Generic

-- | Concurrently reserve a new entity ID.
--
--   __Safety:__ thread-safe.
reserveEntityId :: EntityInfo -> Ecs EntityId
reserveEntityId eInfo = do
  n <- atomicSubIntPVar (eInfo ^. #freeCursor) 1
  if n > 0
    then VR.read (eInfo ^. #pending) (n - 1)
    else tryFrom' . subtract n <$> VR.length (eInfo ^. #meta)

-- | Assert that queued entity IDs have been flushed.
verifyFlushedEntities :: Ecs ()
verifyFlushedEntities = do
  -- TODO disable in release
  eInfo <- view #entities <$> get
  freeCursor <- atomicReadIntPVar $ eInfo ^. #freeCursor
  pendingLen <- VR.length $ eInfo ^. #pending
  unless (freeCursor == pendingLen) (error "this operation requires flushed entities")

-- | Free an entity ID for reuse.
--
--   __Safety:__ not thread-safe.
freeEntityId :: EntityId -> Ecs ()
freeEntityId eId = do
  eInfo <- view #entities <$> get
  verifyFlushedEntities
  -- TODO add invalid ArchRecord
  let pending = eInfo ^. #pending
  VR.push pending eId
  writePVar (eInfo ^. #freeCursor) =<< VR.length pending

-- | Flush pending entity IDs from the queue.
--   Adds new 'ArchRecord's.
--
--   __Safety__: not thread-safe.
flushEntities :: EntityInfo -> Ecs ()
flushEntities eInfo = do
  emptyArch' <- emptyArch

  let len        = eInfo ^. #len
      pending    = eInfo ^. #pending
      meta       = eInfo ^. #meta
      freeCursor = eInfo ^. #freeCursor

  newFreeCursor <- do
    currFreeCursor <- readPVar freeCursor
    if currFreeCursor >= 0
      then pure currFreeCursor
      else do
        -- Allocate new IDs in the entity meta vector.
        oldMetaLen <- VR.length meta
        modifyPVar_ len (+ (-currFreeCursor))
        VU.forM_ [(-currFreeCursor) .. (oldMetaLen + (-currFreeCursor))] \i ->
          -- TODO Would be more efficient to resize the array once and write over
          VR.push meta =<< allocateEntityIntoEmpty (tryFrom' i)
        writePVar freeCursor 0
        pure 0

  pendingLen <- VR.length pending
  modifyPVar_ len (+ (pendingLen - newFreeCursor))
  VU.forM_ [newFreeCursor .. pendingLen] \i -> VR.write meta i
    =<< allocateEntityIntoEmpty (tryFrom' i)

instance {-# OVERLAPPING #-}Default (IO EntityInfo) where
  def = do
    meta <- VR.new
    pending <- VR.new
    freeCursor <- newPVar 0
    len        <- newPVar 0
    pure
      $ MkEntityInfo
      { meta       = meta
      , pending    = pending
      , freeCursor = freeCursor
      , len = len
      }

-- | Reified 'Storage' typeclass dictionary.
--   Used to manipulate untyped stores at runtime.
data StorageDict =
  MkStorageDict
  { _storageInsert :: Any -> EntityId -> Any -> Ecs ()
  , _storageRemove :: Any -> EntityId -> Int -> Ecs ()
  , _storageLookup :: Any -> EntityId -> Int -> Ecs Any
  , _storageModify :: Any -> EntityId -> Int -> Any -> Ecs ()
  , _storageInit   :: !(Ecs Any)
  }
  deriving Generic

-- | Make a reified 'Storage' typeclass dictionary, given a type.
mkStorageDict :: forall a. Storage a => StorageDict
mkStorageDict =
  MkStorageDict
  { _storageInsert = unsafeCoerce @(a -> EntityId -> Elem a -> Ecs ()) @(Any -> EntityId -> Any -> Ecs ())
      $ storageInsert @a
  , _storageRemove = unsafeCoerce @(a -> EntityId -> Int -> Ecs ()) @(Any -> EntityId -> Int -> Ecs ())
      $ storageRemove @a
  , _storageLookup = unsafeCoerce @(a -> EntityId -> Int -> Ecs (Elem a)) @(Any -> EntityId -> Int -> Ecs Any)
      $ storageLookup @a
  , _storageModify = unsafeCoerce @(a -> EntityId -> Int -> Elem a -> Ecs ())
      @(Any -> EntityId -> Int -> Any -> Ecs ())
      $ storageModify @a
  , _storageInit   = unsafeCoerce @(Ecs a) @(Ecs Any) $ storageInit @a
  }

data TypeInfo =
  MkTypeInfo
  { -- | Map a GHC Fingerprint to a unique type identifier.
    types        :: HM.HashMap Fingerprint TypeId
    -- | Next type identifier.
    -- TODO Change to PVar
  , typeCtr      :: IORef TypeId
    -- | Reified typeclass dictionaries for each type @a@ with a @Storage a@ instance.
  , storageDicts :: V.Vector StorageDict
  }
  deriving (Generic)

instance {-# OVERLAPPING #-}Default (IO TypeInfo) where
  def = do
    t <- newIORef (MkTypeId 0)
    pure
      $ MkTypeInfo
      { types        = HM.empty
      , typeCtr      = t
      , storageDicts = V.empty
      }

-- | A cached query entry.
data QueryEntry =
  MkQueryEntry
  { already :: HM.HashMap ArchId Int
  , result  :: SomeQuery
  }
  deriving Generic

instance Default QueryEntry where
  def = MkQueryEntry HM.empty (MkSomeQuery [])

newtype Ecs a =
  MkEcs
  { unEcs :: StateT World IO a -- TODO This really shouldn't be a StateT
  }
  deriving (Functor,Applicative,Monad,MonadState World,MonadIO,MonadThrow,MonadCatch
           ,MonadMask)

data World =
  MkWorld
  { archetypes  :: VR.GrowableIOVector Arch
  , globals     :: VR.GrowableIOVector Any
  , generations :: VR.GrowableIOVector (GUIOVector ArchId)
  , queries     :: HM.HashMap QueryOperator QueryEntry
  , types       :: TypeInfo
  , entities    :: EntityInfo
  }
  deriving Generic

instance {-# OVERLAPPING #-}Default (IO World) where
  def = do
    arc <- VR.new
    glo <- VR.new
    t <- def
    e <- def
    gen <- VR.new
    pure
      $ MkWorld
      { archetypes  = arc
      , globals     = glo
      , types       = t
      , entities    = e
      , generations = gen
      , queries     = HM.empty
      }

class Storage a where
  type Elem a

  -- | Add a new row with a given entity.
  storageInsert :: a -> EntityId -> Elem a -> Ecs ()
  -- | Remove a row at the given index.
  --   This performs a swap-remove on the store-level with the last element.
  --
  --   __Safety:__ the corresponding archetype entity entry must be immediately updated
  --   to reflect the swap-remove.
  storageRemove :: a -> EntityId -> Int -> Ecs ()
  -- | Lookup the row at the given index.
  storageLookup :: a -> EntityId -> Int -> Ecs (Elem a)
  -- | Write a new element to the given index.
  storageModify :: a -> EntityId -> Int -> Elem a -> Ecs ()
  -- | Instantiate a collection of the given type.
  storageInit :: Ecs a

instance PrimMonad Ecs where
  type PrimState Ecs = RealWorld

  primitive = MkEcs . lift . IO

-----------------------------------------------------------------------------------------
data CommandPayload =
  MkCommandPayload
  { typeId :: !TypeId
    -- | Function that commits the transaction to the final archetype.
    --   Takes the final archetype, the target store, and the index (i.e. row) of the entity
    --   (if the store already has a corresponding row).
  , commit :: !(EntityId -> ArchMovement -> Ecs ())
  }
  deriving Generic

data CommandQueue = MkCommandQueue
  { entityId :: !EntityId
  , queue    :: SQ.Seq Command
  }
  deriving Generic

data ArchMovement =
    -- | This 'Command' results in a different archetype. A corresponding row is
    --   added in the target archetype, and the old one in the source archetype is removed.
    MovedArchetype
      { src    :: !ArchRecord
      , dest   :: !ArchRecord
      }
    -- | This 'Command' doesn't move archetypes.
  | SameArchetype !ArchRecord
  deriving Generic

data Command
  = MkTag !CommandPayload
  | MkUntag !CommandPayload
  deriving Generic

-- | Deferred ECS commands.
data Commands =
  MkCommands
  {
  -- { entityId :: EntityId
  -- , commands :: V.Vector Command
  queue    :: SQ.Seq CommandQueue
  }
  deriving Generic

locateEntity :: EntityId -> Ecs ArchRecord
locateEntity eId = do
  ecs <- get
  VR.read (ecs ^. #entities . #meta) (from eId)

getArch :: ArchId -> Ecs Arch
getArch aId = do
  ecs <- get
  VR.read (ecs ^. #archetypes) (from aId)

newtype SystemState =
  MkSystemState
  { commands :: Maybe Commands
  }
  deriving Generic

instance Default SystemState where
  def =
    MkSystemState
    { commands = Nothing
    }

newtype SystemT m a =
  MkSystemT
  { unSystem :: StateT SystemState m a
  }
  deriving (Generic,Functor,Applicative,Monad,MonadIO,MonadState SystemState,MonadTrans
           ,MonadThrow)

runSystem :: System a -> Ecs (a, SystemState)
runSystem sys = runStateT (unSystem sys) def

instance PrimMonad m => PrimMonad (SystemT m) where
  type PrimState (SystemT m) = PrimState m

  primitive = MkSystemT . primitive

type System = SystemT Ecs

emptyArch :: Ecs Arch
emptyArch = do
  ecs <- get
  VR.read (ecs ^. #archetypes) 0

-- | Allocate a new entity into an archetype and updates the requisite stores to match.
--
--   __Safety:__
--
--     * not thread-safe
--
--     * the new 'ArchRecord' must be immediately registered with
--       'EntityInfo'
allocateEntity :: Arch -> EntityId -> V.Vector (Arch -> EntityId -> Int -> ArchMovement -> Ecs ()) -> Ecs ArchRecord
allocateEntity a eId commitHooks = do
  idx <- VR.length (a ^. #entities) <* VR.push (a ^. #entities) eId
  let archRecord = MkArchRecord
        { archId = a ^. #archId
        , idx    = idx
        }
  V.forM_ commitHooks \commitFn -> commitFn a eId idx (MovedArchetype undefined undefined)
  pure archRecord

allocateEntityIntoEmpty :: EntityId -> Ecs ArchRecord
allocateEntityIntoEmpty eId = do
  emptyArch' <- emptyArch
  allocateEntity emptyArch' eId []

-- | Deallocate an entity from an Archetype, given its index in the entities list.
--   This swap-removes the target index with the last element.
deallocateEntity :: Arch -> Int -> Ecs ()
deallocateEntity arch idx = do
  let entities = arch ^. #entities
  n <- VR.pop entities
  case n of
    Just eId -> VR.write entities idx eId
    Nothing -> pure ()

-----------------------------------------------------------------------------------------
derivingUnbox "Edge" [t|Edge -> (ArchId, ArchId)|] [|\(MkEdge a r)
  -> (fromMaybe invalidArchId a, fromMaybe invalidArchId r)|] [|\(a, r)
  -> let convertArchId i =
           if i == invalidArchId
             then Nothing
             else Just i
     in MkEdge (convertArchId a) (convertArchId r)|]

-- TODO This sucks o-wise
typeIdDiff :: VU.Vector TypeId -> VU.Vector TypeId -> VU.Vector TypeId
typeIdDiff a b =
  let buildISet = IS.fromDistinctAscList . VG.toList . VG.map from
      (a', b')  = (buildISet a, buildISet b)
  in VG.map (MkTypeId . fromIntegral) . VG.fromList . IS.toList
     $ IS.union a' b' `IS.difference` IS.intersection a' b'

-- | Provide a unique hash for each type.
class Typeable t => Identify t where
  identify :: (Hash, Fingerprint)
  default identify :: (Hash, Fingerprint)

  --   TODO Generate these w/ TH
  identify = case typeRepFingerprint . typeRep $ Proxy @t of
    f@(Fingerprint h _) -> (fromRight (error "Word size mismatch") (tryFrom h), f)

instance {-# OVERLAPPABLE #-}Typeable t => Identify t

-- | 'Component' specifies which storage structure to use for each type.
--
--   Storage defaults to 'Unboxed' for performance reasons. You must derive
--   'Component via Boxed' if you want Boxed storage.
class (Storage (Layout c), Identify c) => Component (c :: Type) where
  type Layout c

getArchStoreByIdx' :: Arch -> Int -> Maybe Any
getArchStoreByIdx' a idx = (a ^. #components) VG.!? idx

getArchStoreByIdx :: forall c. Component c => Arch -> Int -> Maybe (Layout c)
getArchStoreByIdx a idx = unsafeCoerce @Any @(Layout c) <$> getArchStoreByIdx' a idx

getArchStore' :: Arch -> TypeId -> Maybe Any
getArchStore' a tId = getArchStoreByIdx' a =<< findStoreIdx tId a

getArchStore :: forall c. Component c => Arch -> Ecs (Maybe (Layout c))
getArchStore a = do
  ident <- identified @c
  pure $ (unsafeCoerce @Any @(Layout c)) <$> getArchStore' a ident

reserveAtomic :: Lens' World (IORef c) -> (c -> (c, b)) -> Ecs b
reserveAtomic l f = liftIO . (`atomicModifyIORef` f) . view l =<< get

reserveTypeId :: Ecs TypeId
reserveTypeId =
  reserveAtomic ( #types . #typeCtr) (\t@(MkTypeId i) -> (MkTypeId $ i + 1, t))

-- | Retrieve this type's ID from the global type registry, adding any necessary
--   information to the type registry.
identified :: forall c. Component c => Ecs TypeId
identified = do
  ecs <- get
  case uncurry HM.lookup' (identify @c) (ecs ^. #types . #types) of
    Just tId -> pure tId
    Nothing  -> do
      nextTId <- reserveTypeId
      let (h, f)       = identify @c
          updateTInfo  =
            let addStorageDict v = v `VG.snoc` (mkStorageDict @(Layout c))
                insertTId        = HM.insert' h f nextTId
            in over #types (over #storageDicts addStorageDict . over #types insertTId)
          pushNewEdges = do
            v <- VR.fromGrowable $ ecs ^. #archetypes
            VM.mapM_ (\a -> VR.push (a ^. #edges) (MkEdge Nothing Nothing)) v
          pushGlobals  =
            VR.push (ecs ^. #globals)
            (unsafeCoerce . error
             $ "Tried to access missing global '" <> show (typeRep $ Proxy @c) <> "'.")
      modify updateTInfo >> pushNewEdges >> pushGlobals >> pure nextTId

-- Problems
--
-- 1. Mutable & immutable mixed everywhere
-- 2. Global 'ecs' state
-- 3. Use of StateT when there aren't supposed to be different Worlds laying around
-- 4. Concerns about mutability GC costs


instance Storage (GIOVector c) where
  type Elem (GIOVector c) = c

  -- type Idx (GIOVector c) = Int
  storageInsert v eId e = VR.push v e

  storageRemove v eId idx = do
    -- Moves the last element to fill the gap left by the 'idx' removal
    n <- VR.pop v
    case n of
      Just p  -> VR.write v idx p
      Nothing -> pure ()

  storageLookup v eId idx = VR.read v idx

  storageModify v eId idx = VR.write v idx

  storageInit = VR.new

instance VU.Unbox c => Storage (GUIOVector c) where
  type Elem (GUIOVector c) = c

  -- type Idx (GUIOVector c) = Int
  storageInsert v eId e = VR.push v e

  storageRemove v eId idx = do
    n <- VR.pop v
    case n of
      Just p  -> VR.write v idx p
      Nothing -> pure ()

  storageLookup v eId idx = VR.read v idx

  storageModify v eId idx = VR.write v idx

  storageInit = VR.new

type Boxed c = GIOVector c

type Unboxed c = GUIOVector c

instance {-# OVERLAPPABLE #-}(VU.Unbox c, Identify c) => Component c where
  type Layout c = Unboxed c

insertGlobal :: forall g. Component g => g -> Ecs ()
insertGlobal g = do
  ecs <- get
  globalId <- identified @g
  VR.write (ecs ^. #globals) (from globalId) (unsafeCoerce @g @Any g)

--------------------------------------------------------------------------------
-- | Retrieves a system argument. For queries, this means either loading results
--   from cache or executing a query and caching it.
class Queryable q where
  -- FIXME Redo this whole setup to properly use System
  query :: Ecs q

instance DesugarQuery q => Queryable (Query q) where
  query = do
    res <- desugarQuery @q >>= runQuery
    fmap MkQuery $ V.mapM (\qt -> do
      -- let a = (qt ^. #currArch . #components)
      undefined
      ) . unSomeQuery $ res

-- TODO loosen superclass bound
instance Component c => Queryable (Global c) where
  query = do
    ecs <- get
    globalId <- identified @c
    unsafeCoerce @Any @(Global c) <$> VR.read (ecs ^. #globals) (from globalId)

instance Queryable (Local c) where
  query = undefined

instance Queryable Commands where
  query = undefined

-----------------------------------------------------------------------------------------
-- | Get the return type of a function.
type family Returns s where
  Returns (a -> b) = Returns b
  -- Returns (System a) = (a, Commands)
  Returns (System a) = System a

-- | Saturate a system's arguments as needed.
class Queried s where
  queried :: s -> Ecs (Returns s)

instance (Queryable p, Queried s) => Queried (p -> s) where
  queried s = (s <$> query @p) >>= queried

instance Queried (System a) where
  queried = pure

-----------------------------------------------------------------------------------------
-- TODO For now just one component
nab :: forall c q qi.
    (qi ~ QueryItems q, In qi (Nab c) ~ True, Component c)
    => QueryHandle q
    -> System c
nab qh = do
  targetStore <- lift $ getStore @c qh
  lift $ storageLookup targetStore (qh ^. #entity . _1) (qh ^. #entity . _2)

-----------------------------------------------------------------------------------------
stow :: forall c q qi.
     (qi ~ QueryItems q, In qi (Stow c) ~ True, Component c)
     => QueryHandle q
     -> c
     -> System ()
stow qh c = do
  targetStore <- lift $ getStore @c qh
  lift $ storageModify targetStore (qh ^. #entity . _1) (qh ^. #entity . _2) c

-----------------------------------------------------------------------------------------

newtype CommandBuilder a =
  MkCommandBuilder
  { unCommandBuilder :: StateT CommandQueue System a
  }
  deriving (Generic,Applicative,Functor,Monad,MonadIO
           ,MonadState CommandQueue)

insertionCommit :: forall c. Component c => c -> EntityId -> ArchMovement -> Ecs ()
insertionCommit c eId = \case
    MovedArchetype { src, dest } -> do
      -- We've moved archetypes. Remove and insert.
      let locateStore target = getArch (target ^. #archId) >>= getArchStore @c <&> fromJust
      srcStore <- locateStore src
      destStore <- locateStore dest
      storageInsert destStore eId c
      storageRemove srcStore eId (dest ^. #idx)
    SameArchetype (MkArchRecord { archId, idx }) -> do
      -- We're in the same archetype. Modify.
      targetStore <- getArch archId >>= getArchStore @c <&> fromJust
      storageModify targetStore eId idx c

-- | Add a component to an Entity.
tagged :: forall c. Component c => c -> CommandBuilder ()
tagged c = do
  ident <- MkCommandBuilder $ lift $ lift $ identified @c -- TODO Ugly!
  modify \cq ->
    let tagCommand = MkTag $ MkCommandPayload -- TODO replace w/ insertionCommit + removalCommit
            { typeId = ident
            , commit = insertionCommit c
            }
    in undefined

removalCommit :: forall c. Component c => EntityId -> ArchMovement -> Ecs ()
removalCommit eId = \case
    MovedArchetype { src, dest } -> do
      srcStore <- getArch (src ^. #archId) >>= getArchStore @c <&> fromJust
      storageRemove srcStore eId (dest ^. #idx)
    SameArchetype _ -> pure ()

-- | Remove a component from an Entity.
untagged :: forall c. Component c => CommandBuilder ()
untagged = do
  ident <- MkCommandBuilder $ lift $ lift $ identified @c
  modify \cq ->
    let tagCommand = MkTag $ MkCommandPayload
          { typeId = ident
          , commit = removalCommit @c
          }
    in undefined
  undefined

-- | Despawn an Entity.
despawn :: Commands -> EntityId -> System ()
despawn = undefined

-- | Apply commands to a specific Entity.
onEntity :: Commands -> EntityId -> CommandBuilder () -> System ()
onEntity c eId cb = do
  (_, commandQueue) <- unCommandBuilder cb `runStateT` MkCommandQueue eId SQ.empty
  modify \systemState ->
    MkSystemState
      { commands = Just $
          let commands = fromMaybe (MkCommands SQ.empty) (systemState ^. #commands)
          in MkCommands { queue = (commands ^. #queue) SQ.|> commandQueue}
      }

-- | Spawn a new Entity, then apply the given commands to it.
spawn :: Commands -> CommandBuilder () -> System EntityId
spawn c cb = do
  ecs <- lift get
  newEntityId <- lift . reserveEntityId $ ecs ^. #entities
  onEntity c newEntityId cb
  pure newEntityId

-- tagged c = lift $ do
--   ident <- identified @c
--   let tCommand = \idx ->
--         MkTag
--           { typeId = ident
--           , commit = \arch ->
--               undefined
--               -- let (Just targetIdx) = findStoreidx ident arch
--               --     targetStore = unsafeCoerce @Any @(Layout c) $ (arch ^. #components) VG.! targetIdx
--               --  in maybe (storageInsert targetStore c) (\i -> storageModify targetStore i c) idx
--           }
--   undefined
{-
mySys =
  newId <- command spawn $ do
    tagged $ MkPosition 1.0
    tagged $ MkVelocity 2.0
  ...
-}
-- spawn :: Commands -> System CommandBuilder
-- spawn c = MkCommandBuilder c <$> VR.new <*> VR.new
-- tagged :: forall c. Component c => c -> CommandBuilder -> System CommandBuilder
-- tagged c cb = lift $ do
--   ident <- identified @c
--   let modF = \idx targetArch -> do
--         let (Just targetIdx) = findStoreIdx ident targetArch
--             targetStore = unsafeCoerce @Any @(Layout c) $ (targetArch ^. #components) VG.! targetIdx
--          in maybe (storageInsert targetStore c) (\i -> storageModify targetStore i c) idx
--   VR.push (cb ^. #components) modF
--   VR.push (cb ^. #componentTypes) ident
--   pure cb
-- finish :: CommandBuilder -> System ()
-- finish cb = do
--   modify (set #commands (Just $ cb ^. #commands))
--   VR.push (cb ^. #commands ^. #unCommands) $ MkCommand $ do
--     componentTypes' <- VU.unsafeFreeze =<< VR.fromGrowable (cb ^. #componentTypes)
--     components' <- V.unsafeFreeze =<< VR.fromGrowable (cb ^. #components)
--     targetArch <- traverseRoot $ Add componentTypes'
--     nextIdx <- findNextAvailEntity =<< VR.fromGrowable (targetArch ^. #entities)
--     VG.iforM_ components' (\idx cF -> cF nextIdx targetArch)
--     let archEntities = targetArch ^. #entities
--     case nextIdx of
--       Just idx -> do
--         VR.modify archEntities (set _1 True) idx
--         -- view _2 <$> VR.read archEntities idx
--       Nothing -> do
--         newEntId <- reserveEntityId . view #entities =<< get
--         VR.push archEntities (True, newEntId)
--         -- pure newEntId
destroy = undefined

-----------------------------------------------------------------------------------------
-- | Desugar a type-level query into an isomorphic 'QueryOperator', which can be used to
--   match relevant archetypes.
class DesugarQuery q where
  desugarQuery :: Ecs QueryOperator

instance (DesugarQuery a, DesugarQuery b) => DesugarQuery (a |&| b) where
  desugarQuery = OpAnd <$> desugarQuery @a <*> desugarQuery @b

instance (DesugarQuery a, DesugarQuery b) => DesugarQuery (a ||| b) where
  desugarQuery = OpOr <$> desugarQuery @a <*> desugarQuery @b

instance DesugarQuery q => DesugarQuery (Not q) where
  desugarQuery = OpNot <$> desugarQuery @q

instance Component c => DesugarQuery (Maybe (Nab c)) where
  desugarQuery = OpNab . SubMaybe <$> identified @c

instance Component c => DesugarQuery (Maybe (Stow c)) where
  desugarQuery = OpStow . SubMaybe <$> identified @c

instance Component c => DesugarQuery (Nab c) where
  desugarQuery = OpNab . SubBase <$> identified @c

instance Component c => DesugarQuery (Stow c) where
  desugarQuery = OpStow . SubBase <$> identified @c

instance Component c => DesugarQuery (With c) where
  desugarQuery = OpWith <$> identified @c

-----------------------------------------------------------------------------------------
-- | Whether an archetype is relevant given a query.
--   'Nothing' implies a query has failed. 'Just a' implies that a query has succeeded,
--   mapping a type to an archetype's matching component store.
type QueryRelevant = ReaderT Arch (MaybeT Ecs) (IM.IntMap Any)

-- | Locate the column corresponding to the given type in an archetype's stores.
findStoreIdx :: TypeId -> Arch -> Maybe Int
findStoreIdx tId = VU.findIndex (== tId) . view #types

basicHas :: QuerySubject -> QueryRelevant
basicHas qs = do
  a <- ask
  let targetStore tId = getArchStore' a tId
  case qs of
    SubMaybe tId -> pure . maybe IM.empty (IM.singleton (from tId)) $ targetStore tId
    SubBase tId  -> maybe mzero (pure . IM.singleton (from tId)) $ targetStore tId

processOperator :: QueryOperator -> QueryRelevant
processOperator = \case
  OpNab qs   -> basicHas qs
  OpStow qs  -> basicHas qs
  OpWith tId -> basicHas (SubBase tId) >> pure IM.empty
  OpNot qn   -> do
    res <- lift . lift . runMaybeT . runReaderT (processOperator qn) =<< ask
    maybe (pure IM.empty) (const mzero) res
  OpAnd a b  -> IM.union <$> processOperator a <*> processOperator b
  OpOr a b   -> processOperator a <|> processOperator b

runQueryOperator :: Arch -> QueryOperator -> MaybeT Ecs (IM.IntMap Any)
runQueryOperator a = flip runReaderT a . processOperator

-----------------------------------------------------------------------------------------
runQueryOn :: QueryOperator -> V.Vector Arch -> Ecs SomeQuery
runQueryOn qo as = do
  ecs <- get
  let filterArch (aId, a) = runMaybeT $ do
        queryRes <- runQueryOperator a qo
        nextTId <- liftIO $ from <$> readIORef (ecs ^. #types . #typeCtr)
        let queryEntries = IM.toList queryRes
            matched'     = VG.create $ do
              v <- VGM.new nextTId
              forM_ queryEntries (\(i, a) -> VGM.write v i a)
              pure v
            matchedOk'   = VG.create $ do
              v <- VGM.new nextTId
              forM_ (map fst queryEntries) (\i -> VGM.write v i True)
              pure v
        pure
          $ MkQueryTraversal
          { currArchId = aId
          , currArch   = a
          , matched    = matched'
          , matchedOk  = matchedOk'
          }
  MkSomeQuery
    <$> (VG.mapMaybeM filterArch
         . VG.map (over _1 (MkArchId . fromIntegral))
         . VG.indexed
         $ as)

cacheQuery :: QueryOperator -> SomeQuery -> Ecs ()
cacheQuery qo sq = undefined

runQuery :: QueryOperator -> Ecs SomeQuery
runQuery qo = runQueryOn qo =<< VR.freeze . view #archetypes =<< get

runAndCacheQuery :: QueryOperator -> Ecs SomeQuery
runAndCacheQuery qo = do
  q <- runQuery qo
  cacheQuery qo q
  pure q

getStore :: forall c q. Component c => QueryHandle q -> Ecs (Layout c)
getStore qh = do
  ident <- identified @c
  pure $ unsafeCoerce @Any @(Layout c) $ (qh ^. #trav . #matched) VG.! from ident

-- | Traverse the archetype graph starting at 'root' to find 'as', creating intermediate
--   archetypes as necessary.
traverseArch :: Arch -> ArchSearch -> Ecs Arch
traverseArch root as = do
  ecs <- get
  let (tIds, edgeLens) = case as of
        Add t    -> (t, #add)
        Remove t -> (t, #remove)
      tLen = VG.length tIds
      loop idx root'
        | idx >= tLen = pure root'
        | otherwise = do
          let nextTId = tIds VG.! idx
          targetEdge <- view edgeLens <$> (root' ^. #edges) `VR.read` from nextTId
          nextRoot <- case targetEdge of
            Just r  -> (ecs ^. #archetypes) `VR.read` from r
            Nothing -> createArch
              (VG.modify V.sort $ root ^. #types VG.++ VG.take (idx + 1) tIds)
          loop (idx + 1) nextRoot
  loop 0 root

-- | Traverse the archetype graph starting at the empty archetype to find 'as', creating
--   intermediate archetypes as necessary.
traverseRoot :: ArchSearch -> Ecs Arch
traverseRoot as = do
  ecs <- get
  root <- VR.read (ecs ^. #archetypes) 0
  traverseArch root as

-- | Create a new archetype with the given types.
--
--   The following invariants are /not/ checked:
--     * An archetype with the same types doesn't already exist
--     * Input types are sorted
--
--   __Safety__: not thread-safe.
createArch :: VU.Vector TypeId -> Ecs Arch
createArch tIds = do
  ecs <- get

  liftIO . putStrLn $ "createArch | creating arch of " <> show tIds

  newArchId <- MkArchId . tryFrom' <$> VR.length (ecs ^. #archetypes)
  newArch <- do
    entities' <- VR.new
    edges' <- do
      edgesLen <- liftIO $ from <$> readIORef (ecs ^. #types . #typeCtr)
      replicated <- VUM.replicate edgesLen
        (MkEdge
         { add    = Nothing
         , remove = Nothing
         })
      VR.toGrowable replicated
    components' <- VG.forM (VG.convert tIds) \tId
      -> ((ecs ^. #types . #storageDicts) VG.! from tId) ^. #_storageInit

    pure
      $ MkArch
      { archId     = newArchId
      , components = components'
      , entities   = entities'
      , types      = tIds
      , edges      = edges'
      }
  VR.push (ecs ^. #archetypes) newArch
  -- Configure archetype generations
  untilM (\ct -> pure $ ct > VG.length tIds)
    (\ct -> VR.new >>= VR.push (ecs ^. #generations) >> pure (ct + 1))
    (VR.length $ ecs ^. #generations)
  -- Add to generations list
  VR.read (ecs ^. #generations) (VG.length tIds) >>= (`VR.push` newArchId)
  -- Update incoming & outgoing edges for previous-generation archetypes,
  -- this archetype, and next-generation archetypes.
  populateEdges newArch
  pure newArch

-- | Populate archetype edges from previous-generation archetypes to this archetype,
--   and from this archetype to next-generation archetypes.
--
--   __Safety__: not thread-safe.
populateEdges :: Arch -> Ecs ()
populateEdges arch = do
  ecs <- get
  genCount <- VR.length (ecs ^. #generations)
  let thisGen = VG.length $ arch ^. #types
      nextGen = thisGen + 1
      prevGen = thisGen - 1
      archId = arch ^. #archId
      getGeneration gen = do
        frozenGen <- VR.freeze =<< (ecs ^. #generations) `VR.read` gen
        let readArchetypes   = traverseOf _2 ((view #archetypes ecs `VR.read`) . from)
            convertToArchIds =
              over (mapped . _1) (MkArchId . fromRight undefined . tryFrom)
        VG.mapM (convertToArchIds . readArchetypes)
          . VG.indexed
          . VG.convert @_ @_ @V.Vector
          $ frozenGen

  -- | Write new edge information
  --   'thisDirection' is the edge direction to modify for 'arch'.
  --   'direction' is the edge direction to modify for either a previous- or next-gen
  --   archetype.
  let write direction thisDirection (archId', arch') = do
        let [diff] = (arch' ^. #types) `typeIdDiff` (arch ^. #types)
        VR.modify (arch' ^. #edges) (set direction (Just archId)) (from diff)
        VR.modify (arch ^. #edges) (set thisDirection (Just archId')) (from diff)
  unless (nextGen >= genCount) (getGeneration nextGen >>= VG.mapM_ (write #remove #add))
  unless (prevGen < 0) (getGeneration prevGen >>= VG.mapM_ (write #add #remove))

-- -- | Iterate over all the entities matched by a 'Query'.
-- forQ :: Query q -> (QueryHandle q -> System ()) -> System ()
-- forQ q f = do
--   VG.forM_ (q ^. #unQuery) \qh -> do
--     entities <- VR.fromGrowable (qh ^. #trav . #currArch . #entities)
--     VUM.iforM_ entities \idx (isValid, _) -> when isValid (f $ set #entity idx qh)

-- freeEntity :: EntityId -> Ecs ()
-- freeEntity eId = do
--   ecs <- get
--   record <- VR.read (ecs ^. #entities . #meta) (from eId)
--   targetArch <- VR.read (ecs ^. #archetypes) (from $ record ^. #archId)
--   VG.forM_ (VG.zip (VG.convert $ targetArch ^. #types) (targetArch ^. #components))
--     \(tId, storage)
--     -> let removeFunc =
--              view #_storageRemove $ (ecs ^. #types . #storageDicts) VG.! from tId
--        in removeFunc storage (record ^. #idx)

-- | Locates these Commands' target archetype and commits.
-- processCommands :: Commands -> Ecs ()
-- processCommands commands = do
--   -- First locate the final archetype
--   verifyFlushedEntities
--   rootArch <- getArch . view #archId =<< locateEntity (commands ^. #entityId)
--   let rootTypes = IS.fromDistinctAscList . VG.toList . VG.map from $ rootArch ^. #types
--       v         = commands ^. #commands
--   -- v <- VR.freeze (commands ^. #commands)
--   -- 'commandMap' maps from type ID -> int set of valid 'v' indices
--   let commandMap     = VG.foldl' f IM.empty . VG.indexed $ v
--       (tags, untags) =
--         VG.partition (\case
--                         MkTag _   -> True
--                         MkUntag _ -> False)
--         . VG.ifilter
--         (\idx command
--          -> let isInRootTypes = case command of
--                   MkTag _       -> True
--                   c@(MkUntag _) -> commandTId c `IS.member` rootTypes
--                 isValid       = case IM.lookup (commandTId command) commandMap of
--                   Just is -> IS.member idx is
--                   Nothing -> False
--             in isInRootTypes || isValid)
--         $ v
--   -- Process commands on target archetype
--   targetArch <- do
--     r <- traverseArch rootArch (Remove . VG.convert $ VG.map commandTId' untags)
--     traverseArch r (Add . VG.convert $ VG.map commandTId' tags)
--   newRecord <- allocateEntity targetArch (commands ^. #entityId)
--   let commitCommands c = VG.forM_ c \command -> (getPayload command ^. #commit) targetArch
--         (newRecord ^. #idx)
--   commitCommands tags
--   commitCommands untags
--   where
--     f m (idx, command) =
--       let alterFunc = case command of
--             MkTag p   -> maybe (Just $ IS.singleton idx) (Just . IS.insert idx)
--             MkUntag p -> const (Just $ IS.singleton idx)
--       in IM.alter alterFunc (commandTId command) m

--     getPayload         = \case
--       MkTag p   -> p
--       MkUntag p -> p

--     commandTId'        = \case
--       MkTag (MkCommandPayload {typeId})   -> typeId
--       MkUntag (MkCommandPayload {typeId}) -> typeId

--     commandTId         = from . commandTId'
