{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Target where

import Data.Proxy
import Prelude hiding (Read)
import Control.Monad.Reader (ReaderT, MonadReader, MonadTrans)
import qualified Data.Vector.Unboxed as VU
import Data.Kind
import Recs (Arch)
import GHC.Generics (Generic)
import GHC.TypeLits
import Access

newtype Position = MkPosition (Int, Int)

newtype Velocity = MkVelocity Double

data Flying = MkFlying

data VelocityUpdated = MkVelocityUpdated

data Whatever = MkWhatever

-------------------------------------------------------------------------------

newtype System r w m a = MkSystem { unSystem :: ReaderT ArchTraversal m a}
  deriving (Applicative, Functor, Monad, MonadReader ArchTraversal, MonadTrans)

-- Archetype Iteration
data ArchTraversal = MkArchTraversal
  { types    :: VU.Vector Int
  , currArch :: Arch
  }
  deriving (Generic)

get :: forall c m r w. (Monad m, Read c r) => System r w m c
get = undefined

set :: forall c m r w. (Monad m, Write c w) => c -> System r w m ()
set = undefined

remove :: forall c m r w. (Monad m, Write c w) => c -> System r w m ()
remove = undefined

iWant2 :: (Accesses '[Position, Velocity, Whatever, VelocityUpdated] '[Flying, Velocity, VelocityUpdated] r w, Monad m) => System r w m ()
iWant2 = do
  p <- get @Position
  (MkVelocity v) <- get @Velocity
  if v >= 50
    then get @Whatever >> get @VelocityUpdated >> set MkFlying >> remove MkVelocityUpdated
    else set (MkVelocity $ v + 1.0)

newSys :: System r w m ()
newSys = undefined

parallel :: System r1 w1 m () -> System r2 w2 m () -> System (Merge r1 r2) (ParallelAccess r1 r2 w1 w2) m ()
parallel = undefined
