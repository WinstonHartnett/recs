module Recs.System where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadIO, MonadState, MonadTrans (lift), StateT)
import Recs.Commands
import Recs.World (World)
import Control.Monad.Primitive

-- | System-local state.
data SystemState m = MkSystemState
  { commands :: {-# UNPACK #-} !(Commands m)
  , world :: !World
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

instance PrimMonad m => PrimMonad (SystemT m) where
  type PrimState (SystemT m) = PrimState m

  primitive = MkSystemT . primitive

instance MonadTrans SystemT where
  lift = MkSystemT . lift
