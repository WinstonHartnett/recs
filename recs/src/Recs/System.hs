module Recs.System where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadIO, MonadState, MonadTrans (lift), StateT)
import Recs.Commands
import Recs.Core
import Recs.TypeInfo (TypeInfo)
import Recs.Utils
import GHC.Fingerprint (Fingerprint)

-- | System-local state.
data SystemState m = MkSystemState
  { commands :: {-# UNPACK #-} !(Commands m)
  , newTypeIds :: {-# UNPACK #-} !(GUIOVector (Fingerprint, TypeId))
  , typeInfo :: !TypeInfo
  }

newtype SystemT m a = MkSystemT
  { unSystem :: StateT (SystemState m) m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState (SystemState m)
    , MonadThrow
    )

instance MonadTrans SystemT where
  lift = MkSystemT . lift
