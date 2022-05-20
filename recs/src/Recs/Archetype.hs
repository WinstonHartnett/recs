{-# LANGUAGE OverloadedRecordDot #-}

module Recs.Archetype (Edge(..), Archetype(..), Archetypes(..), getStore', getStoreByIdx') where

-- import Data.Strict.Maybe as SM (Maybe (..), fromMaybe)
import Data.Vector.Unboxed.Deriving

import GHC.Base (Any)
import GHC.Generics (Generic)

import Recs.Core
import Recs.Utils
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Growable as VR
import Effectful
import Effectful.Prim (Prim)
import Recs.EntityInfo (EntityRecord)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Vector.Unboxed as VU

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
    \(MkEdge{add, remove}) ->
      (fromMaybe invalidArchId add, fromMaybe invalidArchId remove)
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
  , entities :: {-# UNPACK #-} !(GUIOVector EntityId)
  , types :: {-# UNPACK #-} !(UVector TypeId)
  , edges :: {-# UNPACK #-} !(GUIOVector Edge)
  }
  deriving (Generic)

newtype Archetypes = MkArchetypes { unArchetypes :: GIOVector Archetype }
  deriving (Generic)

emptyArch :: Prim :> es => Archetypes -> Eff es Archetype
emptyArch a = VR.read a.unArchetypes 0

findStoreIdx :: TypeId -> Archetype -> Maybe Int
findStoreIdx tId arch = VU.findIndex (== tId) $ arch.types

getStoreByIdx' :: Archetype -> Int -> Maybe Any
getStoreByIdx' arch idx = arch.components VG.!? idx

getStore' :: Archetype -> TypeId -> Maybe Any
getStore' a tId = getStoreByIdx' a =<< findStoreIdx tId a

















