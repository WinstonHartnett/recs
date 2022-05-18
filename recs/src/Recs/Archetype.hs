module Recs.Archetype (Edge(..), Archetype(..), Archetypes(..)) where

-- import Data.Strict.Maybe as SM (Maybe (..), fromMaybe)
import Data.Vector.Unboxed.Deriving

import GHC.Base (Any)
import GHC.Generics (Generic)

import Recs.Core
import Recs.Utils
import Data.Maybe (fromMaybe)

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
