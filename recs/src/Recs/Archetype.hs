{-# LANGUAGE OverloadedRecordDot #-}

module Recs.Archetype where

import Data.Vector.Generic qualified as VG
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed qualified as VU
import Effectful
import Effectful.Prim (Prim)
import GHC.Base (Any)
import Recs.Core
import Recs.Types

emptyArch :: Prim :> es => Archetypes -> Eff es Archetype
emptyArch a = VR.read a.unArchetypes 0

findStoreIdx :: TypeId -> Archetype -> Maybe Int
findStoreIdx tId arch = VU.findIndex (== tId) $ arch.types

getStoreByIdx' :: Archetype -> Int -> Maybe Any
getStoreByIdx' arch idx = arch.components VG.!? idx

getStore' :: Archetype -> TypeId -> Maybe Any
getStore' a tId = getStoreByIdx' a =<< findStoreIdx tId a
