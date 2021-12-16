{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}

module Recs.Core where

import           Control.Applicative          (liftA2,liftA3)
import           Control.Lens                 hiding (index)
import           Control.Monad.Catch          (MonadCatch,MonadMask,MonadThrow)
import           Control.Monad.Primitive      (PrimMonad(primitive),PrimState)
import           Control.Monad.Reader
     (MonadIO,MonadReader(ask),MonadTrans(lift),ReaderT)
import           Control.Monad.State          (MonadState,StateT)

import           Data.Bifunctor               (bimap)
import           Data.Generics.Labels
import qualified Data.IntMap                  as IM
import qualified Data.IntSet                  as IS
import qualified Data.Primitive               as P
import           Data.Proxy                   (Proxy,Proxy(..))
import qualified Data.Sequence                as S
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Growable         as VR
import qualified Data.Vector.Unboxed          as VU
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as VUM

import           GHC.Base                     (Any,RealWorld)
import           GHC.Generics                 (Generic)
import           GHC.TypeLits                 (KnownNat,Nat,natVal)

newtype TypeId =
  MkTypeId
  { unTypeId :: VU.Vector Int
  }
  deriving (Generic,Show,Eq)

typeIdDiff :: TypeId -> TypeId -> TypeId
typeIdDiff (MkTypeId a) (MkTypeId b) =
  let set =
        IS.fromDistinctAscList (VG.toList a)
        `IS.difference` IS.fromDistinctAscList (VG.toList b)
  in MkTypeId . VG.fromList . IS.toList $ set

typeIdSubset :: TypeId -> TypeId -> Bool
typeIdSubset (MkTypeId subset) (MkTypeId superset) =
  let set = IS.fromDistinctAscList (VG.toList superset)
  in VG.all (`IS.member` set) subset

newtype EntityId =
  MkEntityId
  { unEntityId :: Int
  }
  deriving Generic

derivingUnbox "EntityId" [t|EntityId -> Int|] [|\(MkEntityId x) -> x|] [|MkEntityId|]

newtype ArchId =
  MkArchId
  { unArchId :: Int
  }
  deriving (Generic,Show,Eq)

derivingUnbox "MkArchId" [t|ArchId -> Int|] [|\(MkArchId x) -> x|] [|MkArchId|]

data Edge =
  MkEdge
  { add    :: !(Maybe ArchId)
  , remove :: !(Maybe ArchId)
  }
  deriving (Generic,Show)

unsafeMkArchId :: Int -> Maybe ArchId
unsafeMkArchId x
  | x < 0 = Nothing
  | otherwise = Just . MkArchId $ x

derivingUnbox "Edge" [t|Edge -> (Int, Int)|] [|\(MkEdge a r)
  -> (maybe (-1) unArchId a, maybe (-1) unArchId r)|] [|\x
  -> let (a, r) = bimap unsafeMkArchId unsafeMkArchId x
   in MkEdge a r|]

data Arch =
  MkArch
  { -- | Component stores in SoA. The 'Any' below depends on the storage method
    --   of the component.
    components :: {-# UNPACK #-} !(V.Vector Any)
    -- | Entity ID of each row element. Used during 'components' traversal.
  , entities   :: VR.GrowableUnboxedIOVector EntityId
    -- | Types stored in this archetype. The order of 'types' matches the order
    --   of 'components'.
  , types      :: {-# UNPACK #-} !TypeId
    -- | Edges used to traverse the archetype graph. There is one edge per type
    --   in the ECS.
  , edges      :: VUM.IOVector Edge
  }
  deriving Generic

data Ecs =
  MkEcs
  { archetypes  :: VR.GrowableIOVector Arch
  , registry    :: P.MutVar RealWorld TypeRegistry
  , generations :: VR.GrowableIOVector (VR.GrowableUnboxedIOVector ArchId)
  }
  deriving Generic

data ArchRecord =
  MkArchRecord
  { archId :: ArchId
  , row    :: Int
  }
  deriving (Generic,Show)

data TypeRegistry =
  MkTypeRegistry
  { known       :: IS.IntSet
  , ct          :: Int
  , entityIndex :: IM.IntMap ArchRecord
  }
  deriving (Generic,Show)

type family Elem s

class (Elem (Storage c) ~ c) => Component c where
  type Storage c

  typeId :: Proxy c -> TypeId

newtype EcsT m a =
  MkEcsT
  { unEcs :: ReaderT Ecs m a
  }
  deriving (Applicative,Functor,Monad,MonadReader Ecs,MonadIO,MonadTrans,MonadThrow
           ,MonadCatch,MonadMask)

instance PrimMonad m => PrimMonad (EcsT m) where
  type PrimState (EcsT m) = PrimState m

  primitive = lift . primitive

class (KnownNat (Identified t)) => Identify t where
  type Identified t :: Nat

  identify :: Proxy t -> Int
  default identify :: (KnownNat (Identified t)) => Proxy t -> Int
  {-# INLINE identify #-}
  identify _ = fromIntegral . natVal $ Proxy @(Identified t)

-------------------------------------------------------------------------------
-- Store Management
-------------------------------------------------------------------------------
class Get m s where
  index :: s -> Int -> EcsT m (Elem s)

class Set m s where
  set' :: s -> Elem s -> EntityId -> EcsT m ()

class Exists m c where
  exists :: Proxy c -> EntityId -> EcsT m Bool

class Destroy m s where
  destroy :: s -> EntityId -> EcsT m ()

class Members st m s where
  members :: Proxy st -> s -> EcsT m (VU.Vector EntityId)

getRecord :: EntityId -> EcsT IO (Maybe ArchRecord)
getRecord eId = do
  ecs <- ask
  P.readMutVar (ecs ^. #registry) <&> IM.lookup (unEntityId eId) . view #entityIndex

-- | Locate the archetype that holds this Entity ID.
followRecord :: ArchRecord -> EcsT IO Arch
followRecord record = ask >>= \ecs -> (ecs ^. #archetypes)
  `VR.read` unArchId (record ^. #archId)

followEntityId :: EntityId -> EcsT IO (Maybe Arch)
followEntityId eId = getRecord eId >>= traverse followRecord

type instance Elem (a, b) = (Elem a, Elem b)

instance (Monad m, Get m a, Get m b) => Get m (a, b) where
  index (a, b) eId = liftA2 (,) (index a eId) (index b eId)

type instance Elem (a, b, c) = (Elem a, Elem b, Elem c)

instance (Monad m, Get m a, Get m b, Get m c) => Get m (a, b, c) where
  index (a, b, c) eId = liftA3 (,,) (index a eId) (index b eId) (index c eId)

type instance Elem (a, b, c, d) = (Elem a, Elem b, Elem c, Elem d)

instance (Monad m, Get m a, Get m b, Get m c, Get m d) => Get m (a, b, c, d) where
  index (a, b, c, d) eId = do
    a' <- index a eId
    b' <- index b eId
    c' <- index c eId
    d' <- index d eId
    pure (a', b', c', d')

-------------------------------------------------------------------------------
-- Archetype Traversal
-------------------------------------------------------------------------------
-- | 'QueryContent' stores the read and write accesses of a system along with a
--   'valid' function that can be applied to an archetype to determine whether
--   it is of interest to a system.
--
--   For example, this rudimentary query adds a check that this archetype does
--   not contain a @Velocity@ store:
--
--   @
--       let isNotVelocity =
--             \a -> pure $ VG.any (== identify (Proxy @Velocity)) (a ^. #types)
--       modify (over #valid (\v -> \a -> liftA2 (&&) (v a) (isNotVelocity a)))
--   @
--
--   = Safety
--
--   You should generally use primitive query operators (like 'read' and 'write')
--   instead of directly accessing archetypes. The above example can result in a
--   race condition.
data QueryContent m =
  MkQueryContent
  { -- | 'reads' is only used for system access control.
    reads  :: S.Seq Int
    -- | 'writes' is only used for system access control.
  , writes :: S.Seq Int
    -- | When 'valid' returns @False@, the current archetype is skipped, and
    --   traversal continues from the next in sequence. Note that you should
    --   not ever mutate the ECS from within a query! This @m@ here should be
    --   used wisely.
  , valid  :: Arch -> m Bool
  }
  deriving Generic

instance Monad m => Semigroup (QueryContent m) where
  (MkQueryContent r w v) <> (MkQueryContent r' w' v') =
    MkQueryContent (r <> r') (w <> w') (\a -> liftA2 (&&) (v a) (v' a))

instance Monad m => Monoid (QueryContent m) where
  mempty = MkQueryContent S.empty S.empty (const (pure True))

-- | A 'Query' has two important jobs:
--
--   1. Track the components read and written to by a system.
--   2. Determine if this system should run on an archetype, if at all.
--
--   == Access Control
--
--   A 'Query' has two levels of access control. Its type-level information
--   is used for static verification (i.e. in the blueprint builder). It also
--   stores the same as runtime information in its 'QueryContent' 's 'reads' and
--   'writes' fields. The latter allows for unscheduled or foreign systems to
--   run safely.
--
--   == Filtering
--
--   Other ECSes have two separate notions: whether an archetype meets a
--   system's criteria and whether a query should be run at all (called a
--   "run criterion"). A 'Query' combines both.
--
--   Use primitives like 'read', 'write', and 'optional' to filter archetypes.
--   Use 'halt' to cancel an entire system (see 'halt' for caveats and safety
--   properties).
--
--   TODO This is jank (come up w/ better querying primitives). Fix so you can:
--
--   @
--       do
--         read @(Position, Velocity)
--         optional @Whatever
--         write @Flying
--         customQueryCached (do
--           arch <- ask
--           if VU.any (== 1) (arch ^. #types)
--             then skip
--             else pure ())
--   @
newtype Query x m a =
  MkQuery
  { unQuery :: StateT (QueryContent m) QueryIO a
  }
  deriving (Generic,Applicative,Functor,Monad,MonadState (QueryContent m))

newtype QueryIO a =
  MkQueryIO
  { runQueryIO :: IO a
  }
  deriving (Applicative,Functor,Monad,MonadIO)

-- TODO
-- instance Alternative (Query x m a) where
--   empty = MkQuery
data ArchTraversal =
  MkArchTraversal
  { currArch   :: Arch
  , currArchId :: ArchId
    -- | 'matched' maps a single type ID to the index of the relevant component
    --   store in the archetype.
  , matched    :: VU.Vector Int
  , currEntity :: EntityId
  }
  deriving Generic

newtype SystemT x m a =
  MkSystem
  { unSystem :: ReaderT ArchTraversal m a
  }
  deriving (Generic,Applicative,Functor,Monad,MonadReader ArchTraversal,MonadTrans)

data Blueprint x m a =
  MkBlueprint
  { query  :: Query x m a
  , system :: SystemT x m a
  }
  deriving Generic
