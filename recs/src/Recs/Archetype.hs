module Recs.Archetype (Edge, Archetype, Archetypes) where

import Data.Strict.Maybe as SM (Maybe (..), fromMaybe)
import Data.Vector.Unboxed.Deriving

import GHC.Base (Any)
import GHC.Generics (Generic)

import Recs.Core
import Recs.Utils

-- | An edge in the Archetype graph.
data Edge = MkEdge
  { add :: !(SM.Maybe ArchId)
  , remove :: !(SM.Maybe ArchId)
  }
  deriving (Generic, Show)

derivingUnbox
  "Edge"
  [t|Edge -> (ArchId, ArchId)|]
  [|
    \(MkEdge{add, remove}) ->
      (SM.fromMaybe invalidArchId add, SM.fromMaybe invalidArchId remove)
    |]
  [|
    \(add, remove) ->
      let convertArchId i
            | invalidArchId == i = SM.Nothing
            | otherwise = SM.Just i
       in MkEdge (convertArchId add) (convertArchId remove)
    |]

data Archetype = MkArch
  { components :: {-# UNPACK #-} !(Vector Any)
  , entities :: {-# UNPACK #-} !(GUIOVector EntityId)
  , types :: {-# UNPACK #-} !(UVector TypeId)
  , edges :: {-# UNPACK #-} !(GUIOVector Edge)
  }
  deriving (Generic)

newtype Archetypes = MkArchetypes (GIOVector Archetype)
  deriving (Generic)
