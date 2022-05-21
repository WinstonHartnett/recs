{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Recs.Archetype where

import Data.Coerce (coerce)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Growable qualified as VR
import Data.Vector.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Effectful
import Effectful.Prim (Prim)
import Effectful.State.Static.Local (get)
import GHC.Base (Any)
import Recs.TypeInfo (identified, pendingTypeId)
import Recs.Types
import Recs.Utils
import Unsafe.Coerce (unsafeCoerce)

mkArchetype :: Ecs es => UVector TypeId -> Eff es Archetype
mkArchetype tVec = do
  ecs <- get @World
  let tVec' = VG.convert tVec'
  components' <- liftIO $ VG.mapM _storageInit $ VG.backpermute ecs.typeInfo.storageDicts tVec'
  entities <- VR.withCapacity 0
  typeMap <- do
    typesLen <- from <$> pendingTypeId
    VR.thaw $
      coerce $
        VU.update (VU.replicate typesLen invalidTypeId) $
          VU.map (\(compIdx, tId) -> (from tId, from compIdx)) $ VU.indexed tVec
  edges <- VR.new
  pure
    MkArch
      { components = components'
      , entities = entities
      , types = tVec
      , typeMap = typeMap
      , edges = edges
      }

emptyArch :: Prim :> es => Archetypes -> Eff es Archetype
emptyArch a = VR.read a.archetypes 0

-- | Locate the column corresponding to the given type in an archetype's stores.
findStoreIdx :: TypeId -> Archetype -> Maybe Int
findStoreIdx tId arch = VU.findIndex (== tId) $ arch.types

getStoreByIdx' :: Archetype -> Int -> Maybe Any
getStoreByIdx' arch idx = arch.components VG.!? idx

getStore' :: Archetype -> TypeId -> Maybe Any
getStore' a tId = getStoreByIdx' a =<< findStoreIdx tId a

unsafeGetStore :: forall c es. (Component c, Ecs es) => Archetype -> Eff es (Maybe c)
unsafeGetStore a = unsafeCoerce @(Maybe Any) @(Maybe c) <$> getStore' a <$> identified @c
