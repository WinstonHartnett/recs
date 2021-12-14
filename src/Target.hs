{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Target where

import           Control.Monad.Reader (MonadReader,MonadTrans,ReaderT)
import           Control.Monad.Writer (MonadWriter,WriterT)
import Control.Monad (when)
import Control.Applicative (liftA3)

import Control.Lens hiding (index)

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

newtype Position = MkPosition Double

instance Component Position where
  type Storage Position = UMap Position
instance Identify Position

newtype Velocity = MkVelocity Double

instance Component Velocity where
  type Storage Velocity = UMap Velocity
instance Identify Velocity

data Flying = MkFlying

instance Component Flying where
  type Storage Flying = BMap Flying
instance Identify Flying

data VelocityUpdated = MkVelocityUpdated

instance Component VelocityUpdated where
  type Storage VelocityUpdated = BMap VelocityUpdated
instance Identify VelocityUpdated

data Player = MkPlayer

instance Component Player where
  type Storage Player = BMap Player
instance Identify Player

data Whatever = MkWhatever

instance Component Whatever where
  type Storage Whatever = BMap Whatever
instance Identify Whatever

data A

instance Component A where
  type Storage A = BMap A
instance Identify A

data B

instance Component B where
  type Storage B = BMap B
instance Identify B

data C

instance Component C where
  type Storage C = BMap C
instance Identify C

newtype Time = MkTime Double
  deriving (Num)

instance Component Time where
  type Storage Time = UMap Time

instance Identify Time

data QueryContent =
  MkQueryContent
  { with'     :: VU.Vector Int
  , without'  :: VU.Vector Int
  , branched' :: VU.Vector Int
  }
  deriving Generic

instance Semigroup QueryContent where
  (MkQueryContent a b c) <> (MkQueryContent d e f) =
    MkQueryContent (a <> d) (b <> e) (c <> f)

instance Monoid QueryContent where
  mempty = MkQueryContent VU.empty VU.empty VU.empty

newtype Query x m a =
  MkQuery
  { unQuery :: WriterT QueryContent m a
  }
  deriving (Generic,Applicative,Functor,Monad,MonadWriter QueryContent)

data ArchTraversal = MkArchTraversal
  { currArch   :: Arch
  , currArchId :: ArchId
    -- | 'matched' maps a single type ID to the index of the relevant component
    --   store in the archetype.
  , matched    :: VU.Vector Int
  , currEntity :: EntityId
  }
  deriving Generic

newtype System x m a =
  MkSystem
  { unSystem :: ReaderT ArchTraversal m a
  }
  deriving (Generic,Applicative,Functor,Monad,MonadReader ArchTraversal)

data Blueprint x m a =
  MkBlueprint
  { query  :: Query x m a
  , system :: System x m a
  }
  deriving Generic

type family AddRead l x where
  AddRead '[] x = x
  AddRead a '(r, w) = '(Merge a r, w)

type family AddWrite l x where
  AddWrite '[] x = x
  AddWrite a '(r, w) = '(r, Merge a w)

type family Merge a b where
  Merge (a ': as) b = a ': Merge as b
  Merge '[] b = b

type family UnTuple x where
  UnTuple (a, b, c) = '[a, b, c]
  UnTuple (a, b) = '[a, b]
  UnTuple a = '[a]

class Read c x

class Write c x

type family ElemOrError i x where
  ElemOrError i (i ': as) = 'True
  ElemOrError i (a ': as) = ElemOrError i as
  ElemOrError i '[] = TypeError ('Text "Using undeclared dependency ‘" :<>: ShowType i :<>: 'Text "’ in system.\n"
                                  :<>: 'Text "    • Hint: Add either 'read @"
                                  :<>: ShowType i :<>: 'Text "' or 'write @"
                                  :<>: ShowType i :<>: 'Text "'\n            to your system's dependencies.")

type family ComponentToReads2 c x :: Constraint where
  ComponentToReads2 '[] x = ()
  ComponentToReads2 (a ': as) x = (ElemOrError a x ~ 'True, ComponentToReads2 as x)

type family ComponentToReads c x :: Constraint where
  ComponentToReads c x = ComponentToReads2 (UnTuple c) x

type family MapTuple (f :: Type -> Constraint) l :: Constraint where
  MapTuple _ '[] = ()
  MapTuple f (a ': as) = (f a, MapTuple f as)

type family Snd x where
  Snd '(_, b) = b

type family Fst x where
  Fst '(a, _) = a

emap :: Query x m a -> System x m a -> Blueprint x m a
emap = MkBlueprint

read :: forall c m a x. (MapTuple Identify (UnTuple c), Monad m) => Query '( UnTuple c, '[] ) m a
read = undefined

write :: forall c m a x. (MapTuple Identify (UnTuple c), Monad m) => Query '( UnTuple c, UnTuple c ) m a
write = undefined

optional :: forall c m a x. (MapTuple Identify (UnTuple c), Monad m) => Query '( UnTuple c, '[] ) m a
optional = undefined

with :: forall c m a x. (ComponentToReads c (Fst x), MapTuple Identify (UnTuple c), Monad m) => System x m c
with = undefined

branch :: forall c m x. (MapTuple Identify (UnTuple c), Monad m) => System x m ()
branch = undefined

tag :: forall c m x. (MapTuple Identify (UnTuple c), Monad m) => c -> System x m ()
tag = undefined

untag :: forall c m x. (MapTuple Identify (UnTuple c), Monad m) => System x m ()
untag = undefined

nothing :: Monad m => Query '( '[], '[] ) m a
nothing = undefined

(-.-) :: Query '(x, x') m a -> Query '(y, y') m a -> Query '(Merge x y, Merge x' y') m a
(-.-) = undefined

-- | 2 pieces:
--
--   1. query: which archetypes and components this system operates on
--         * determines access to components
--   2. system: the behavior given those targets
mySystem =
  emap (read @(Position, Velocity) @IO -.- optional @Whatever -.- write @Flying)
      (do MkVelocity v <- with
          when (v >= 50) (branch @Whatever >> tag MkFlying))

mySystem2 =
  emap (optional @Whatever @IO -.- write @(Velocity, Flying, VelocityUpdated))
       (do MkVelocity v <- with
           if v >= 50 then branch @(Whatever, VelocityUpdated)
                             >> tag MkFlying
                             >> untag @VelocityUpdated
                      else tag (MkVelocity $ v + 1.0))

stepPosition =
  emap (read @(Velocity, Time) @IO -.- write @Position)
       (do
         MkTime dT    <- with
         MkPosition p <- with
         MkVelocity v <- with
         tag (MkPosition $ p + dT * v))

class Identify t where
  identify :: Proxy t -> Int

class Component c => GetA st c where
  getA :: Proxy st -> ArchTraversal -> EcsT IO c

pullArchetype :: ArchTraversal -> Int -> Any
pullArchetype trav tId =
  let archIndex = (trav ^. #matched) VU.! tId
    in (trav ^. #currArch . #components) V.! archIndex

instance (Get IO (Storage a), Identify a, Component a) => GetA Archetypal a where
  getA _ trav = do
    let tId = identify $ Proxy @a
        targetStore = unsafeCoerce @_ @(Storage a) $ pullArchetype trav tId
    index targetStore (unEntityId $ trav ^. #currEntity)

instance (Get IO (Storage a), Identify a, Component a) => GetA Flat a where
  getA _ trav = do
    let tId = identify $ Proxy @a
    undefined
