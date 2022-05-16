module Recs.Commands where

import Recs.Lib (ArchRecord)
import Recs.Core (TypeId, EntityId)

data ArchetypeMove
  = MovedArchetype
      { src :: {-# UNPACK #-} !ArchRecord
      , dest :: {-# UNPACK #-} !ArchRecord
      }
  | SameArchetype {-# UNPACK #-} !ArchRecord

data Command m
  = MkTag
      { typeId :: !TypeId
      , commit :: !(EntityId -> ArchetypeMove -> m ())
      }


















