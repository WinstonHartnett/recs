{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Recs.Archetype where

import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Heap qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed qualified as VU
import Effectful
import Effectful.Prim (Prim)
import Effectful.State.Static.Local (get)
import GHC.Base (Any)
import Recs.TypeInfo (identified, pendingTypeId)
import Recs.Types
import Recs.Utils
import Unsafe.Coerce (unsafeCoerce)

{- | Make a new archetype.
   This does not make the new archetype valid. Use 'createArchetype' instead.
-}
mkArchetype :: Ecs es => UVector TypeId -> Eff es Archetype
mkArchetype tVec = do
  ecs <- get @World
  newArchId <- from <$> VR.length ecs.archetypes.archetypes
  components' <-
    liftIO
      . VG.mapM _storageInit
      . VG.backpermute ecs.typeInfo.storageDicts
      . VG.convert
      $ VG.map from tVec
  entities <- VR.withCapacity 0
  typeMap <- do
    typesLen <- from <$> pendingTypeId
    let typeMap' =
          VU.update (VU.replicate typesLen invalidTypeId)
            . VU.map (\(compIdx, tId) -> (from tId, from compIdx))
            $ VU.indexed tVec
    VR.thaw . coerce $ typeMap'
  edges <- VR.new
  pure
    MkArch
      { archId = newArchId
      , components = components'
      , entities = entities
      , types = tVec
      , typeMap = typeMap
      , edges = edges
      }

emptyArch :: Ecs es => Eff es Archetype
emptyArch = do
  ecs <- get @World
  VR.read (ecs.archetypes.archetypes) 0

-- | Locate the column corresponding to the given type in an archetype's stores.
findStoreIdx :: TypeId -> Archetype -> Maybe Int
findStoreIdx tId arch = VU.findIndex (== tId) $ arch.types

hasStore :: Prim :> es => TypeId -> Archetype -> Eff es Bool
hasStore tId arch = do
  idx <- VR.read arch.typeMap $ coerce tId
  pure $ idx == (-1)

getStoreByIdx :: Archetype -> Int -> Maybe Any
getStoreByIdx arch idx = arch.components VG.!? idx

getStoreByIdx' :: Archetype -> Int -> Any
getStoreByIdx' arch idx = arch.components VG.! idx

getStoreByTypeId :: Ecs es => Archetype -> TypeId -> Eff es (Maybe Any)
getStoreByTypeId arch tId = getStoreByIdx arch <$> VR.read arch.typeMap (from tId)

getStoreByTypeId' :: Ecs es => Archetype -> TypeId -> Eff es Any
getStoreByTypeId' arch tId = getStoreByIdx' arch <$> VR.read arch.typeMap (from tId)

unsafeGetStore :: forall c es. (Component c, Ecs es) => Archetype -> Eff es (Maybe (Layout c))
unsafeGetStore arch = identified @c >>= getStoreByTypeId arch <&> unsafeCoerce @(Maybe Any) @(Maybe (Layout c))

unsafeGetStore' :: forall c es. (Component c, Ecs es) => Archetype -> Eff es (Layout c)
unsafeGetStore' arch = identified @c >>= getStoreByTypeId' arch <&> unsafeCoerce @Any @(Layout c)

{- | Ensure that the Archetype graph has a corresponding entry for Archetypes of
   length 'nTypes'.
-}
reserveGenerations :: Ecs es => Int -> Eff es ()
reserveGenerations nTypes = do
  ecs <- get @World
  let pushGenerations ct =
        unless
          (ct > nTypes)
          (VR.new >>= VR.push ecs.archetypes.graph >> pushGenerations (ct + 1))
  pushGenerations =<< VR.length ecs.archetypes.graph

-- | Number of generations in the archetype graph.
generationCount :: Ecs es => Eff es Int
generationCount = do
  ecs <- get @World
  VR.length $ ecs.archetypes.graph

readGeneration :: Ecs es => Int -> Eff es (UVector ArchId)
readGeneration gen = get @World >>= (`VR.read` gen) . view (#archetypes . #graph) >>= VR.freeze

archetypeGeneration :: Archetype -> Int
archetypeGeneration arch = VG.length $ arch.types

{- | Populate archetype edges from previous-generation archetypes to this archetype,
   and from this archetype to next-generation archetypes.
-}
populateEdges :: Ecs es => Archetype -> Eff es ()
populateEdges arch = do
  ecs <- get @World
  genCt <- generationCount
  let thisGen = archetypeGeneration arch
      nextGen = thisGen + 1
      prevGen = thisGen - 1
      getGeneration gen = do
        frozenGen <- readGeneration gen
        let readArchetypes = traverseOf _2 \aId -> ecs.archetypes.archetypes `VR.read` from aId
            convertToArchIds = over (mapped . _1) from
        VG.mapM (convertToArchIds . readArchetypes)
          . VG.indexed
          . VG.convert @_ @_ @V.Vector
          $ frozenGen
  let write direction thisDirection (archId', arch') = do
        let diff = case arch.types `typeIdDiff` arch.types of
              [i] -> i
              _ -> undefined
        VR.modify arch'.edges (set direction (Just arch.archId)) (from diff)
        VR.modify arch.edges (set thisDirection (Just archId')) (from diff)
  unless (nextGen >= genCt) (getGeneration nextGen >>= VG.mapM_ (write #remove #add))
  unless (prevGen < 0) (getGeneration prevGen >>= VG.mapM_ (write #add #remove))

-- | Register an 'Archetype' in the graph.
registerInGraph :: Ecs es => Archetype -> Eff es ()
registerInGraph arch = do
  ecs <- get @World
  let tIds = arch.types
  reserveGenerations . VG.length $ tIds
  VR.read ecs.archetypes.graph (VG.length tIds) >>= (`VR.push` arch.archId)
  populateEdges arch

createArchetype :: Ecs es => UVector TypeId -> Eff es Archetype
createArchetype tIds = do
  ecs <- get @World
  newArch <- mkArchetype tIds
  VR.push ecs.archetypes.archetypes newArch
  -- Add our archetype to the generation graph.
  registerInGraph newArch
  pure newArch

traverseArch :: Ecs es => Archetype -> ArchetypeSearch -> Eff es Archetype
traverseArch root search = do
  let (tIds, edgeLens) =
        case search of
          Add t -> (t, #add)
          Remove t -> (t, #remove)
      tLen = VG.length tIds
      go idx root'
        | idx >= tLen = pure root'
        | otherwise = do
            let nextTId = tIds VG.! idx
            targetEdge <- view edgeLens <$> root.edges `VR.read` from nextTId
            nextRoot <- case targetEdge of
              Just r -> do
                ecs <- get @World
                VR.read ecs.archetypes.archetypes (from r)
              Nothing ->
                createArchetype
                  . VG.modify V.sort
                  $ root.types VG.++ VG.take (idx + 1) tIds
            go (idx + 1) nextRoot
  go 0 root

traverseRoot :: Ecs es => ArchetypeSearch -> Eff es Archetype
traverseRoot as = (`traverseArch` as) =<< emptyArch
