{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Target where

import           Control.Monad.Reader (MonadReader,MonadTrans,ReaderT)
import           Control.Monad.Writer (MonadWriter,WriterT)
import Control.Monad (when)
import Control.Applicative (liftA2, liftA3)

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

-- | This stores the read and write accesses of a system along with a 'valid'
--   function that can be applied to an archetype to determine whether it is
--   of interest to a system.
--
--   For example, this rudimentary query adds a check that this archetype does
--   not contain a @Velocity@ store:
--
--   @
--       let isNotVelocity =
--             \a -> pure $ VG.any (== identify (Proxy @Velocity)) (a ^. #types)
--       modify (over #valid (\v -> \a -> liftA2 (&&) (v a) (isNotVelocity a)))
--   @
--
--   = Safety
--
--   You should generally use primitive query operators (like 'read' and 'write')
--   instead of directly accessing archetypes. The above example can result in a
--   race condition.
data QueryContent m =
  MkQueryContent
  { -- | 'reads' is only used for system access control.
    reads     :: S.Seq Int
    -- | 'writes' is only used for system access control.
  , writes    :: S.Seq Int
    -- | When 'valid' returns @False@, the current archetype is skipped, and
    --   traversal continues from the next in sequence. Note that you should
    --   not ever mutate the ECS from within a query! This @m@ here should be
    --   used wisely.
  , valid     :: Arch -> m Bool
  }
  deriving Generic

instance Monad m => Semigroup (QueryContent m) where
  (MkQueryContent r w v) <> (MkQueryContent r' w' v') =
    MkQueryContent (r <> r') (w <> w') (\a -> liftA2 (&&) (v a) (v' a))

instance Monad m => Monoid (QueryContent m) where
  mempty = MkQueryContent S.empty S.empty (const (pure True))

-- | A 'Query' has two important jobs:
--
--   1. Track the components read and written to by a system.
--   2. Determine if this system should run on an archetype, if at all.
--
--   == Access Control
--
--   A 'Query' has two levels of access control. Its type-level information
--   is used for static verification (i.e. in the blueprint builder). It also
--   stores the same as runtime information in its 'QueryContent' 's 'reads' and
--   'writes' fields. The latter allows for unscheduled or foreign systems to
--   run safely.
--
--   == Filtering
--
--   Other ECSes have two separate notions: whether an archetype meets a
--   system's criteria and whether a query should be run at all (called a
--   "run criterion"). A 'Query' combines both.
--
--   Use primitives like 'read', 'write', and 'optional' to filter archetypes.
--   Use 'cancel' to cancel an entire system (see 'cancel' for caveats
--   and safety properties).
newtype Query x m a =
  MkQuery
  { unQuery :: StateT (QueryContent m) m a
  }
  deriving (Generic,Applicative,Functor,Monad,MonadState (QueryContent m))

-- instance Alternative (Query x m a) where
--   empty = MkQuery

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

type family AssertIn c x :: Constraint where
  AssertIn '[] x = ()
  AssertIn (a ': as) x = (ElemOrError a x ~ 'True, AssertIn as x)

type family MapGetA m x :: Constraint where
  MapGetA m (i ': as) = (MapGetA2 (Layout (Storage i)) i,
                         Get m (Storage i),
                         Identify i,
                         Component i,
                         MapGetA m as)
  MapGetA _ '[] = ()

type family MapGetA2 l i where
  MapGetA2 Archetypal i = GetA Archetypal i
  MapGetA2 Flat i = GetA Flat i

-- type family MapGet m i x :: Constraint where
--   MapGet _ _ '[] = ()
--   MapGet m i

-- type family AssertIn c x :: Constraint where
--   AssertIn c x = AssertIn2 (UnTuple c) x

type family MapTuple (f :: Type -> Constraint) l :: Constraint where
  MapTuple f (a ': as) = (f a, MapTuple f as)
  MapTuple _ '[] = ()

type family Map (f :: Type -> Type) l where
  Map f (a ': as) = f a ': Map f as
  Map _ '[] = '[]

type family Snd x where
  Snd '(_, b) = b

type family Fst x where
  Fst '(a, _) = a

class IdentifyFromList l where
  identifyFromList :: Proxy l -> S.Seq Int

instance (Identify a, IdentifyFromList as) => IdentifyFromList (a ': as) where
  identifyFromList _ = identify (Proxy @a) S.<| identifyFromList (Proxy @as)
{-# SPECIALIZE identifyFromList :: Proxy (a ': as) -> S.Seq Int #-}

instance IdentifyFromList '[] where
  identifyFromList _ = []
{-# SPECIALIZE identifyFromList :: Proxy '[] -> S.Seq Int #-}

emap :: Query x m a -> System x m a -> Blueprint x m a
emap = MkBlueprint

-- | Read from archetypes with 'c'.
read :: forall c m a x t. (t ~ UnTuple c, IdentifyFromList t, Monad m) => Query '( t, '[] ) m ()
read = modify (over #reads (<> (identifyFromList $ Proxy @t)))

-- | Write to archetypes with 'c'.
write :: forall c m a x t. (t ~ UnTuple c, IdentifyFromList t, Monad m) => Query '( t, t ) m ()
write = modify
  (over #writes (<> (identifyFromList $ Proxy @t)) .
   over #reads  (<> (identifyFromList $ Proxy @t)))

-- | Read from archetypes that might have 'c'.
optional :: forall c m a x t. (t ~ UnTuple c, IdentifyFromList t, Monad m) => Query '( Map Maybe t, '[] ) m ()
optional = unsafeCoerce $ read @c @m

-- | Short-circuit querying and prevent the attached system from running
--   until the next ECS loop.
halt :: forall m x. Query x m ()
halt = undefined

-- | Construct a custom query, allowing unrestricted access to the 'Query'
--   state while querying. The query result is cached.
cachedCustomQuery :: forall m x a. Query x m a -> Query x m a
cachedCustomQuery = undefined

-- | Construct a custom query that is cached after its first use.
--   See 'customQuery' for safety and caveats.
uncachedCustomQuery :: forall m x a. Query x m a -> Query x m a
uncachedCustomQuery = undefined

with :: forall c m a x t.
  (t ~ UnTuple c,
   AssertIn t (Fst x),
   MapGetA m t,
   Monad m)
  => System x m c
with = undefined

branch :: forall c m x t. (MapTuple Identify (UnTuple c), Monad m) => System x m ()
branch = undefined

tag :: forall c m x t. (MapTuple Identify (UnTuple c), Monad m) => c -> System x m ()
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
  type Identified t :: Nat
  identify :: Proxy t -> Int
  default identify :: (KnownNat (Identified t)) => Proxy t -> Int
  {-# INLINE identify #-}
  identify _ = fromIntegral . natVal $ Proxy @(Identified t)

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

type family PropStore s where
  PropStore (a, b, c) = (PropStore a, PropStore b, PropStore c)
  PropStore (a, b) = (PropStore a, PropStore b)
  PropStore a = Storage a

class GetSA s where
  getSA :: ArchTraversal -> EcsT IO (PropStore s)

instance (GetSA a, GetSA b) => GetSA (a, b) where
  getSA trav = do
    a <- getSA @a trav
    b <- getSA @b trav
    pure (a, b)

instance (PropStore a ~ Storage a, Identify a, Component a) => GetSA a where
  getSA trav = do
    let aIdent = identify $ Proxy @a
    pure $ unsafeCoerce @_ @(Storage a) $ pullArchetype trav aIdent

with' :: forall a. (GetSA a, Get IO (PropStore a)) => ArchTraversal -> EcsT IO (Elem (PropStore a))
with' trav = do
  targetStore <- getSA @a trav
  targetElem <- index targetStore 0
  pure $ targetElem





