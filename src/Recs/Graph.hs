module Recs.Graph where

import           Control.Lens
import           Control.Monad               (unless)
import           Control.Monad.Reader        (ask)

import           Recs.Core

import           Data.Generics.Labels
import qualified Data.Primitive              as P
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Growable        as VR
import qualified Data.Vector.Unboxed.Mutable as VUM

import           GHC.Generics                (Generic)

data ArchSearch
  = AddType
  | SubType
  deriving (Generic,Show)

traverseArch :: Arch -> TypeId -> ArchSearch -> EcsT IO Arch
traverseArch root tId@(MkTypeId t) search = do
  ecs <- ask
  let archetypes      = ecs ^. #archetypes
      edgeLens        = case search of
        AddType -> #add
        SubType -> #remove
      -- 'loop' traverses the archetype graph until it reaches the desired
      -- archetype specified by 'tId'. Creates new archetypes as needed.
      loop tId' root' = case VG.uncons tId' of
        Just (nextTId, stack) -> do
          targetEdge <- view edgeLens <$> (root' ^. #edges) `VUM.read` nextTId
          nextRoot <- case targetEdge of
            Just (MkArchId r) -> (ecs ^. #archetypes) `VR.read` r
              -- Nothing -> createArch $ tId `typeIdDiff` MkTypeId tId'
          loop stack nextRoot
        Nothing -> pure root'
  loop t root

traverseRoot :: TypeId -> EcsT IO Arch
traverseRoot tId = do
  ecs <- ask
  root <- (ecs ^. #archetypes) `VR.read` 0
  traverseArch root tId AddType

createArch :: TypeId -> EcsT IO Arch
createArch tId@(MkTypeId t) = do
  ecs <- ask
  newArchId <- MkArchId <$> VR.length (ecs ^. #archetypes)
  newArch <- MkArch (VG.generate (VG.length t) (const undefined)) <$> VR.new
    <*> pure tId
    <*> (P.readMutVar (ecs ^. #registry) >>= VUM.new . view #ct)
  (ecs ^. #archetypes) `VR.push` newArch
  (ecs ^. #generations) `VR.read` VG.length t
    >>= (`VR.push` newArchId) -- Add to generation list
  -- Update incoming & outgoing edges for previous-generation archetypes,
  -- this archetype, and next-generation archetypes.
  populateEdges (newArchId, newArch)
  pure newArch

populateEdges :: (ArchId, Arch) -> EcsT IO ()
populateEdges (archId, arch) = do
  ecs <- ask
  genCount <- VR.length (ecs ^. #generations)
  let thisGen           = VG.length . unTypeId $ arch ^. #types
      nextGen           = thisGen + 1
      prevGen           = thisGen - 1
      getGeneration gen =
        (ecs ^. #generations) `VR.read` gen
        >>= VR.freeze
        >>= VG.mapM (over (mapped . _1) MkArchId
                     . traverseOf _2 (((ecs ^. #archetypes) `VR.read`) . unArchId))
        . VG.indexed
        . (VG.convert @_ @_ @V.Vector)
  nextArches <- if nextGen >= genCount
    then pure []
    else getGeneration nextGen
  prevArches <- getGeneration prevGen
  -- | Write new edge information
  --   'thisDirection' is the edge direction to modify for 'arch'.
  --   'direction' is the edge direction to modify for either a previous- or next-gen
  --   archetype.
  let write direction thisDirection (archId', arch') = do
        let diff = VG.head . unTypeId $ (arch' ^. #types) `typeIdDiff` (arch ^. #types)
        VUM.modify (arch' ^. #edges) (set direction (Just archId)) diff
        VUM.modify (arch ^. #edges) (set thisDirection (Just archId')) diff
  unless (nextGen >= genCount) (VG.forM_ nextArches (write #remove #add))
  VG.forM_ prevArches (write #add #remove)
