{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Recs where

import Control.Lens
import Control.Monad ( unless, when )
import Control.Monad.Reader ( ReaderT (runReaderT), ask, lift, MonadIO (liftIO), MonadReader, MonadTrans )
import Control.Monad.State ( StateT )
import Data.Data ( Proxy (Proxy) )
import Data.Default ( Default(def) )
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Maybe ( fromJust, fromMaybe, isNothing )
import qualified Data.Primitive as P
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Growable as VR
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving ( derivingUnbox )
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Base ( Any, RealWorld, IO (IO) )
import GHC.Generics ( Generic )
import Unsafe.Coerce ( unsafeCoerce )
import Witch ()
import Control.Monad.Primitive (PrimMonad (PrimState, primitive), PrimBase, MonadPrim)
import Control.Monad.Catch (MonadThrow, MonadMask, MonadCatch)
import Data.IORef (IORef)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe cond x
    | cond x = Just x
    | otherwise = Nothing

--------------------------------------------------------------------------------
-- Example Components
--------------------------------------------------------------------------------
newtype Position = MkPosition ( Double, Double )
    deriving ( Generic, Show )

derivingUnbox "Position" [t| Position -> ( Double, Double ) |]
  [| \(MkPosition x) -> x |] [| MkPosition |]

newtype Velocity = MkVelocity Double
    deriving ( Generic, Show )

derivingUnbox "Velocity" [t| Velocity -> Double |] [| \(MkVelocity x) -> x |]
  [| MkVelocity |]

data Flying = MkFlying
    deriving ( Generic, Show )

--------------------------------------------------------------------------------
-- Ecs Data
--------------------------------------------------------------------------------
-- | A combination of unique type IDs. This newtype is guaranteed sorted.
newtype TypeId = MkTypeId { unTypeId :: VU.Vector Int }
    deriving ( Generic, Show, Eq )

typeIdDiff :: TypeId -> TypeId -> TypeId
typeIdDiff (MkTypeId a) (MkTypeId b)
    = let set = IS.fromDistinctAscList (VG.toList a)
              `IS.difference` IS.fromDistinctAscList (VG.toList b)
    in MkTypeId . VG.fromList . IS.toList $ set

typeIdSubset :: TypeId -> TypeId -> Bool
typeIdSubset (MkTypeId subset) (MkTypeId superset)
    = let set = IS.fromDistinctAscList (VG.toList superset) in VG.all
        (`IS.member` set) subset

-- | Unique Entity ID.
--   A 'MkEntityId -1' is invalid.
newtype EntityId = MkEntityId { unEntityId :: Int }
    deriving ( Generic, Show, Eq )

derivingUnbox "EntityId" [t| EntityId -> Int |] [| \(MkEntityId x) -> x |]
  [| MkEntityId |]

-- | Archetype ID used to index into the global archetypes array.
--   A 'MkArchId -1' is invalid.
newtype ArchId = MkArchId { unArchId :: Int }
    deriving ( Generic, Show, Eq )

derivingUnbox "MkArchId" [t| ArchId -> Int |] [| \(MkArchId x) -> x |]
  [| MkArchId |]

data Edge = MkEdge { add :: !(Maybe ArchId), remove :: !(Maybe ArchId) }
    deriving ( Generic, Show )

unsafeMkArchId :: Int -> Maybe ArchId
unsafeMkArchId x
    | x < 0 = Nothing
    | otherwise = Just . MkArchId $ x

derivingUnbox "Edge" [t| Edge -> ( Int, Int ) |]
  [| \(MkEdge a r) -> ( maybe (-1) unArchId a, maybe (-1) unArchId r ) |]
  [| \x -> let ( a, r ) = bimap unsafeMkArchId unsafeMkArchId x in MkEdge a r |]

class Component c where
    type Storage c
    typeId :: Proxy c -> TypeId

data Arch
    = MkArch {
          -- | Component stores in SoA. The 'Any' below depends on the storage method
          --   of the component.
          components :: {-# UNPACK #-} !(V.Vector Any),
          -- | Entity ID of each row element. Used during 'components' traversal.
          entities :: VR.GrowableUnboxedIOVector EntityId,
          -- | Types stored in this archetype. The order of 'types' matches the order
          --   of 'components'.
          types :: {-# UNPACK #-} !TypeId,
          -- | Edges used to traverse the archetype graph. There is one edge per type
          --   in the ECS.
          edges :: VUM.IOVector Edge }
    deriving Generic

data ArchRecord = MkArchRecord { archId :: ArchId, row :: Int }
    deriving ( Generic, Show )

-- | Metadata about global ECS types.
data TypeRegistry = MkTypeRegistry { known :: IS.IntSet, ct
                        :: Int, entityIndex :: IM.IntMap ArchRecord }
    deriving ( Generic, Show )

instance Default TypeRegistry where
    def = MkTypeRegistry IS.empty 0 IM.empty

data Ecs = MkEcs { archetypes :: VR.GrowableIOVector Arch, registry
               :: P.MutVar RealWorld TypeRegistry, generations
               :: VR.GrowableIOVector (VR.GrowableUnboxedIOVector ArchId) }
    deriving Generic

instance Default (IO Ecs) where
    def = do
        --
        archetypes <- VR.withCapacity 1
        rootArch <- MkArch V.empty <$> VR.new <*> pure (MkTypeId [])
            <*> VUM.new 0
        VR.write archetypes 0 rootArch
        --
        registry <- P.newMutVar def
        --
        generations <- VR.withCapacity 1
        rootGen <- VR.withCapacity 1
        VR.write rootGen 0 (MkArchId 0)
        VR.write generations 0 rootGen
        --
        pure $ MkEcs archetypes registry generations

newtype SystemT m a = MkSystemT { unSystemT :: ReaderT Ecs m a }
  deriving (Functor, Applicative, Monad, MonadReader Ecs, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadMask)

type System a = SystemT IO a

runSystemT :: SystemT m a -> Ecs -> m a
runSystemT = runReaderT . unSystemT

instance PrimMonad m => PrimMonad (SystemT m) where
  type PrimState (SystemT m) = PrimState m
  primitive = lift . primitive

--------------------------------------------------------------------------------
-- Graph Traversal
--------------------------------------------------------------------------------
data ArchSearch = AddType | SubType
    deriving ( Generic, Show )

traverseArch :: Arch -> TypeId -> ArchSearch -> System Arch
traverseArch root tId@(MkTypeId t) search = do
    ecs <- ask
    let archetypes = ecs ^. #archetypes
        edgeLens = case search of
            AddType -> #add
            SubType -> #remove
        -- 'loop' traverses the archetype graph until it reaches the desired
        -- archetype specified by 'tId'. Creates new archetypes as needed.
        loop tId' root' = case VG.uncons tId' of
            Just ( nextTId, stack ) -> do
                targetEdge <- view edgeLens <$> (root' ^. #edges)
                    `VUM.read` nextTId
                nextRoot <- case targetEdge of
                    Just (MkArchId r) -> (ecs ^. #archetypes) `VR.read` r
                    -- Nothing -> createArch $ tId `typeIdDiff` MkTypeId tId'
                loop stack nextRoot
            Nothing -> pure root'
    loop t root

traverseRoot :: TypeId -> System Arch
traverseRoot tId = do
    ecs <- ask
    root <- (ecs ^. #archetypes) `VR.read` 0
    traverseArch root tId AddType

createArch :: TypeId -> System Arch
createArch tId@(MkTypeId t) = do
    ecs <- ask
    newArchId <- MkArchId <$> VR.length (ecs ^. #archetypes)
    newArch <- MkArch (VG.generate (VG.length t) (const undefined)) <$> VR.new
        <*> pure tId
        <*> (P.readMutVar (ecs ^. #registry) >>= VUM.new . view #ct)
    (ecs ^. #archetypes) `VR.push` newArch
    (ecs ^. #generations) `VR.read` VG.length t >>= (`VR.push` newArchId) -- Add to generation list
    -- Update incoming & outgoing edges for previous-generation archetypes,
    -- this archetype, and next-generation archetypes.
    populateEdges ( newArchId, newArch )
    pure newArch

populateEdges :: ( ArchId, Arch ) -> System ()
populateEdges ( archId, arch ) = do
    ecs <- ask
    genCount <- VR.length (ecs ^. #generations)
    let thisGen = VG.length . unTypeId $ arch ^. #types
        nextGen = thisGen + 1
        prevGen = thisGen - 1
        getGeneration gen = (ecs ^. #generations) `VR.read` gen >>= VR.freeze
            >>= VG.mapM (over (mapped . _1) MkArchId . traverseOf _2
                         (((ecs ^. #archetypes) `VR.read`) . unArchId))
            . VG.indexed . (VG.convert @_ @_ @V.Vector)
    nextArches <- if nextGen >= genCount then pure [] else getGeneration nextGen
    prevArches <- getGeneration prevGen
    -- | Write new edge information
    --   'thisDirection' is the edge direction to modify for 'arch'.
    --   'direction' is the edge direction to modify for either a previous- or next-gen
    --   archetype.
    let write direction thisDirection ( archId', arch' ) = do
            let diff = VG.head . unTypeId $ (arch' ^. #types)
                    `typeIdDiff` (arch ^. #types)
            VUM.modify (arch' ^. #edges) (set direction (Just archId)) diff
            VUM.modify (arch ^. #edges) (set thisDirection (Just archId')) diff
    unless (nextGen >= genCount) (VG.forM_ nextArches (write #remove #add))
    VG.forM_ prevArches (write #add #remove)

--------------------------------------------------------------------------------
-- Stores
--------------------------------------------------------------------------------

newtype UMap c = MkUMap (VR.GrowableUnboxedIOVector c)
newtype BMap c = MkBMap (VR.GrowableIOVector c)
newtype Global c = MkGlobal (IORef c)
data Tagged c = MkTagged

class ConvertTuple a where
  unsafeConvertTuple :: V.Vector Any -> a

instance ConvertTuple a where
  unsafeConvertTuple [x] = unsafeCoerce x

instance ConvertTuple (a, b) where
  unsafeConvertTuple [x, y] = (unsafeCoerce x, unsafeCoerce y)

instance ConvertTuple (a, b, c) where
  unsafeConvertTuple [x, y, z] = (unsafeCoerce x, unsafeCoerce y, unsafeCoerce z)

componentExists :: forall k c. Component c => Proxy c -> EntityId -> SystemT IO Bool
componentExists p eId = do
  archRec <- getRecord eId
  case archRec of
    Just archRec' -> do
      targetArch <- followRecord archRec'
      let tId = typeId p
          isSubset = tId `typeIdSubset` (targetArch ^. #types)
      row <- (targetArch ^. #entities) `VR.read` (archRec' ^. #row)
      pure $ row /= MkEntityId (-1) && isSubset
    Nothing -> pure False

componentGet :: forall c. (Component c, ConvertTuple (Storage c)) => EntityId -> SystemT IO c
componentGet eId = do
  targetArch <- fromJust <$> followEntityId eId
  let tId = typeId $ Proxy @c
      targetVec = VG.backpermute (targetArch ^. #components) (VG.convert . unTypeId $ tId)
      converted = unsafeConvertTuple @(Storage c) targetVec

  undefined

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

class Store s where
  type Elem s

--------------------------------------------------------------------------------
-- Component Management
--
-- These methods are NOT checked for race conditions. Do not use them in
-- systems!
--------------------------------------------------------------------------------
class Component c => Get m c where
  -- | Whether a given entity exists in a store somewhere.
  exists :: Proxy c -> EntityId -> SystemT m Bool
  -- | Get an entity by its unique ID.
  entity :: EntityId -> SystemT m c

class Component c => Set m c where
  set' :: c -> EntityId -> SystemT m ()

class Component c => Destroy m c where
  destroy :: Proxy c -> EntityId -> SystemT m ()

class Component c => Members m c where
  members :: Proxy c -> SystemT m (VU.Vector EntityId)

--------------------------------------------------------------------------------
-- Entity Management
--------------------------------------------------------------------------------

getRecord :: EntityId -> System (Maybe ArchRecord)
getRecord eId = do
  ecs <- ask
  P.readMutVar (ecs ^. #registry) <&> IM.lookup (unEntityId eId) . view #entityIndex

-- | Locate the archetype that holds this Entity ID.
followRecord :: ArchRecord -> System Arch
followRecord record = ask >>= \ecs -> (ecs ^. #archetypes) `VR.read` unArchId (record ^. #archId)

followEntityId :: EntityId -> System (Maybe Arch)
followEntityId eId = getRecord eId >>= traverse followRecord

someFunc :: IO ()
someFunc = putStrLn "someFunc"
