{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Target where

import           Control.Monad.Reader (MonadReader(ask),MonadTrans,ReaderT, lift)
import           Control.Monad.Writer (MonadWriter,WriterT, MonadIO)
import Control.Monad (when)
import Control.Applicative (liftA2, liftA3)
import Control.Arrow

import Control.Lens hiding (index,Contains)
import TypeLevel

import           Data.Generics.Labels
import           Data.Kind
import           Data.Proxy
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import qualified Data.Vector.Generic  as VG
import qualified Data.Vector.Growable as VR

import           GHC.Base             (Any)
import           GHC.Generics         (Generic)
import           GHC.TypeLits

import           Prelude              hiding (Read,read)

import           Core
import           Stores
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (MonadState, modify)
import Control.Monad.State.Lazy (StateT)
import Data.Coerce (coerce)

import qualified Data.Sequence as S

newtype Position = MkPosition Double

instance Component Position where
  type Storage Position = UMap Position
instance Identify Position where
  type Identified Position = 6

newtype Velocity = MkVelocity Double

instance Component Velocity where
  type Storage Velocity = UMap Velocity
instance Identify Velocity where
  type Identified Velocity = 5

data Flying = MkFlying

instance Component Flying where
  type Storage Flying = BMap Flying
instance Identify Flying where
  type Identified Flying = 4

data VelocityUpdated = MkVelocityUpdated

instance Component VelocityUpdated where
  type Storage VelocityUpdated = BMap VelocityUpdated
instance Identify VelocityUpdated where
  type Identified VelocityUpdated = 3

data Player = MkPlayer

instance Component Player where
  type Storage Player = BMap Player
instance Identify Player where
  type Identified Player = 2

data Whatever = MkWhatever
instance Component Whatever where
  type Storage Whatever = BMap Whatever
instance Identify Whatever where
  type Identified Whatever = 0
newtype Time = MkTime Double
  deriving (Num)

instance Component Time where
  type Storage Time = UMap Time

instance Identify Time where
  type Identified Time = 1

class IdentifyFromList l where
  identifyFromList :: Proxy l -> S.Seq Int

instance (Identify a, IdentifyFromList as) => IdentifyFromList (a ': as) where
  identifyFromList _ = identify (Proxy @a) S.<| identifyFromList (Proxy @as)
{-# SPECIALIZE identifyFromList :: Proxy (a ': as) -> S.Seq Int #-}

instance IdentifyFromList '[] where
  identifyFromList _ = []
{-# SPECIALIZE identifyFromList :: Proxy '[] -> S.Seq Int #-}

emap :: Query x m a -> SystemT x m a -> Blueprint x m a
emap = MkBlueprint

-- | Read from archetypes with 'c'.
read :: forall c m a x t. (t ~ TupleToList c, IdentifyFromList t, Monad m) => Query '( t, '[] ) m ()
read = modify (over #reads (<> (identifyFromList $ Proxy @t)))

-- | Write to archetypes with 'c'.
write :: forall c m a x t. (t ~ TupleToList c, IdentifyFromList t, Monad m) => Query '( t, t ) m ()
write = modify
  (over #writes (<> (identifyFromList $ Proxy @t)) .
   over #reads  (<> (identifyFromList $ Proxy @t)))

-- | Read from archetypes that might have 'c'.
optional :: forall c m a x t. (t ~ TupleToList c, IdentifyFromList t, Monad m) => Query '( Map Maybe t, '[] ) m ()
optional = unsafeCoerce $ read @c @m

-- | Short-circuit querying and prevent the attached system from running
--   until the next ECS loop.
halt :: forall m x. Query x m ()
halt = undefined

-- | Construct a custom query, allowing unrestricted access to the 'Query'
--   state. The query result is cached.
cachedCustomQuery :: forall m x a. Query x m a -> Query x m a
cachedCustomQuery = undefined

-- | Construct a custom query, allowing unrestricted access to the 'Query'
--   state. The query result is not cached. This can result in poor
--   performance if the query is particularly expensive. Generally  prefer
--   'cachedCustomQuery' over this.
uncachedCustomQuery :: forall m x a. Query x m a -> Query x m a
uncachedCustomQuery = undefined

data AccessType
  = ReadAccess
  | WriteAccess

type family ShowAccess t where
  ShowAccess 'ReadAccess = 'Text "read"
  ShowAccess 'WriteAccess = 'Text "write"

type family AssertEachIn c x t :: Constraint where
  AssertEachIn (a ': as) x t =
    (IfC
      (Contains a x)
      ()
      ( TypeError ('Text "Using undeclared "
                  :<>: ShowAccess t
                  :<>: 'Text " dependency ‘"
                  :<>: ShowType a
                  :<>:  'Text "’ in system.\n"
                  :<>: 'Text "    • Hint: Add ‘"
                  :<>: ShowAccess t
                  :<>: 'Text " @"
                  :<>: ShowType a
                  :<>: 'Text "’ to your system's dependencies."
                  )))
  AssertEachIn '[] _ _ = ()

type HasBase c =
  ( Elem (PropStore c) ~ c
  , Get IO (PropStore c)
  , Locate c (IsTuple c))

type SelectAccess l t = If (Equal t 'ReadAccess) (Fst l) (Snd l)

type Has c l t =
  (HasBase c,
  AssertEachIn (TupleToList c) (SelectAccess l t) t)

type Identifiable c = ToConstraint (Map Identify (TupleToList c))

with :: forall c x. Has c x 'ReadAccess => SystemT x (EcsT IO) c
with = ask >>= lift . locate @c @(IsTuple c) >>= lift . (`index` 0)

type HasBranch c l t =
  (HasBase c,
  AssertEachIn (Map Maybe (TupleToList c)) (SelectAccess l t) t)

branch :: forall c x. HasBranch c x 'ReadAccess => SystemT x (EcsT IO) ()
branch = undefined

tag :: forall c m x t. (Has c x 'WriteAccess, Identifiable c, Monad m) => c -> SystemT x m ()
tag = undefined

untag :: forall c m x. (Has c x 'WriteAccess, Identifiable c, Monad m) => SystemT x m ()
untag = undefined

nothing :: Monad m => Query '( '[], '[] ) m a
nothing = undefined

(-.-) :: Query '(x, x') m a -> Query '(y, y') m a -> Query '(x ++ y, x' ++ y') m a
(-.-) = undefined

-- | 2 pieces:
--
--   1. query: which archetypes and components this system operates on
--         * determines access to components
--   2. system: the behavior given those targets
mySystem =
  emap (read @(Position, Velocity) -.- optional @Whatever -.- write @Flying)
       (do MkVelocity v <- with
           when (v >= 50) (branch @Whatever >> tag MkFlying))

mySystem2 =
  emap (optional @Whatever -.- write @(Velocity, Flying, VelocityUpdated))
       (do MkVelocity v <- with
           w <- with @(Maybe Whatever)
           if v >= 50 then branch @(Whatever, VelocityUpdated)
                             >> tag MkFlying
                             >> untag @VelocityUpdated
                      else tag (MkVelocity $ v + 1.0))

stepPosition =
  emap
    (read @(Velocity, Time) -.- write @Position)
    (with >>= \(MkTime dT, MkPosition p, MkVelocity v) -> tag (MkPosition $ p + dT * v))

