module Core where

import           Control.Lens                 hiding (index)
import           Control.Monad.Catch          (MonadCatch,MonadMask,MonadThrow)
import           Control.Monad.Primitive      (PrimMonad(primitive),PrimState)
import           Control.Monad.Reader
     (MonadIO,MonadReader(ask),MonadTrans(lift),ReaderT)

import           Data.Bifunctor               (bimap)
import           Data.Generics.Labels
import qualified Data.IntMap                  as IM
import qualified Data.IntSet                  as IS
import qualified Data.Primitive               as P
import           Data.Proxy                   (Proxy)
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Growable         as VR
import qualified Data.Vector.Unboxed          as VU
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as VUM

import           GHC.Base                     (Any,RealWorld)
import           GHC.Generics                 (Generic)
import Control.Applicative (liftA2, liftA3)

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

class Get m s where
  index :: s -> Int -> EcsT m (Elem s)

class Set m s where
  set' :: s -> Elem s -> EntityId -> EcsT m ()

class Exists st m c where
  exists :: Proxy st -> Proxy c -> EntityId -> EcsT m Bool

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

-------------------------------------------------------------------------------
-- Get instances
-------------------------------------------------------------------------------

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
