{-# LANGUAGE OverloadedRecordDot #-}

module Recs.Commands where

import Control.Monad.State.Strict (MonadState, StateT)
import Data.Sequence qualified as SQ
import Recs.Core (EntityId, TypeId)
import Recs.Lib (ArchRecord)
import Recs.World (World)
import GHC.Fingerprint (Fingerprint)

data ArchetypeMove
  = MovedArchetype
      { src :: {-# UNPACK #-} !ArchRecord
      , dest :: {-# UNPACK #-} !ArchRecord
      }
  | SameArchetype {-# UNPACK #-} !ArchRecord

data Command m
  = MkTag
      { fingerprint :: !Fingerprint
      , commit :: !(World -> m ())
      }
  | MkUntag
      { fingerprint :: !Fingerprint
      , commit :: !(World -> m ())
      }
  | Despawn

data EntityCommands m = MkEntityCommands
  { entityId :: !EntityId
  , queue :: SQ.Seq (Command m)
  }

newtype Commands m = MkCommands {unCommands :: SQ.Seq (EntityCommands m)}

newtype CommandBuilder m a = MkCommandBuilder
  { unCommandBuilder :: StateT (EntityCommands m) m a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadState (EntityCommands m)
    )





















