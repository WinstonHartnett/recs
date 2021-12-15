{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Target where

import           Control.Monad.Reader (MonadReader(ask),MonadTrans,ReaderT, lift)
import           Control.Monad.Writer (MonadWriter,WriterT, MonadIO)
import Control.Monad (when)
import Control.Applicative (liftA2, liftA3)
import Control.Arrow

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

-- import Fcf

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
--
--   TODO This is jank (come up w/ better querying primitives). Fix so you can:
--
--   @
--       do
--         read @(Position, Velocity)
--         optional @Whatever
--         write @Flying
--         customQueryCached (do
--           arch <- ask
--           if VU.any (== 1) (arch ^. #types)
--             then skip
--             else pure ())
--   @
newtype Query x m a =
  MkQuery
  { unQuery :: StateT (QueryContent m) QueryIO a
  }
  deriving (Generic,Applicative,Functor,Monad, MonadState (QueryContent m))

newtype QueryIO a = MkQueryIO { runQueryIO :: IO a }
  deriving (Applicative, Functor, Monad, MonadIO)

-- TODO
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

newtype SystemT x m a =
  MkSystem
  { unSystem :: ReaderT ArchTraversal m a
  }
  deriving (Generic,Applicative,Functor,Monad,MonadReader ArchTraversal, MonadTrans)

data Blueprint x m a =
  MkBlueprint
  { query  :: Query x m a
  , system :: SystemT x m a
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

emap :: Query x m a -> SystemT x m a -> Blueprint x m a
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
--   state. The query result is cached.
cachedCustomQuery :: forall m x a. Query x m a -> Query x m a
cachedCustomQuery = undefined

-- | Construct a custom query, allowing unrestricted access to the 'Query'
--   state. The query result is not cached. This can result in poor
--   performance if the query is particularly expensive. Generally  prefer
--   'cachedCustomQuery' over this.
uncachedCustomQuery :: forall m x a. Query x m a -> Query x m a
uncachedCustomQuery = undefined

type family CanLocate c where
  CanLocate c = (Elem (PropStore c) ~ c, Locate c (IsTuple c))

type family Has c l :: Constraint where
  Has c l = (AssertIn (UnTuple c) (Fst l), CanLocate c, Get IO (PropStore c))

with :: forall c x. Has c x => SystemT x (EcsT IO) c
with = ask >>= lift . locate @c @(IsTuple c) >>= lift . (`index` 0)

branch :: forall c x. Has c x => SystemT x (EcsT IO) ()
branch = undefined

tag :: forall c m x t. (MapTuple Identify (UnTuple c), Monad m) => c -> SystemT x m ()
tag = undefined

untag :: forall c m x. (MapTuple Identify (UnTuple c), Monad m) => SystemT x m ()
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
  emap (read @(Position, Velocity) -.- optional @Whatever -.- write @Flying)
       (do MkVelocity v <- with
           when (v >= 50) (branch @Whatever >> tag MkFlying))

mySystem2 =
  emap (optional @Whatever -.- write @(Velocity, Flying, VelocityUpdated))
       (do MkVelocity v <- with
           if v >= 50 then branch @(Whatever, VelocityUpdated)
                             >> tag MkFlying
                             >> untag @VelocityUpdated
                      else tag (MkVelocity $ v + 1.0))

stepPosition =
  emap
    (read @(Velocity, Time) -.- write @Position)
    (with >>= \(MkTime dT, MkPosition p, MkVelocity v) -> tag (MkPosition $ p + dT * v))

class Identify t where
  type Identified t :: Nat
  identify :: Proxy t -> Int
  default identify :: (KnownNat (Identified t)) => Proxy t -> Int
  {-# INLINE identify #-}
  identify _ = fromIntegral . natVal $ Proxy @(Identified t)

type family PropStore s where
  PropStore (a, b, c) = (PropStore a, PropStore b, PropStore c)
  PropStore (a, b) = (PropStore a, PropStore b)
  PropStore a = Storage a

-- | 'Pull' specifies how to retrieve a single store during archetype iteration
--    based on their 'Layout' type.
--
--    For example, 'Archetypal' layouts pull from the current archetype's
--    stores, whereas 'Flat' layouts are pulled from global (i.e. not stored in
--    in archetype graph) stores regardless of the current archetype.
class (Identify c, Component c) => Pull layout c where
  pull :: Proxy layout -> ArchTraversal -> EcsT IO (Storage c)
{-# SPECIALIZE pull :: (Identify c, Component c) => Proxy Archetypal -> ArchTraversal -> EcsT IO (Storage c) #-}
{-# SPECIALIZE pull :: (Identify c, Component c) => Proxy Flat -> ArchTraversal -> EcsT IO (Storage c) #-}

instance (Identify c, Component c) => Pull Archetypal c where
  pull _ trav =
    let storeIdx = (trav ^. #matched) VU.! identify (Proxy @c)
        pulled = (trav ^. #currArch . #components) V.! storeIdx
      in pure . unsafeCoerce @_ @(Storage c) $ pulled

instance (Identify c, Component c) => Pull Flat c where
  -- pull _ _ = unsafeCoerce @_ @(Storage a) <$> pullFlat (identify $ Proxy @a)
  pull _ _ = undefined

type family IsTuple t where
  IsTuple (a, b, c) = 'True
  IsTuple (a, b) = 'True
  IsTuple a = 'False

-- | 'Locate' is used to pull a store for each member of a tuple. For example:
--
--   @
--      (a :: Storage Velocity, (b :: Storage Time, c :: Storage Whatever))
--          <- locate @(Velocity, (Time, Whatever)) trav
--   @
--
--   This is then passed to 'Get', after which we have our target components:
--
--   @
--      (x :: (Storage Velocity, (Storage Time, Storage Whatever)))
--          <- locate @(Velocity, (Time, Whatever)) trav
--      (MkVelocity v, (MkTime t, MkWhatever)) <- index x (myEntityId)
--   @
--
--   == Locate Signature
--
--   The 'it' type variable is a trick that helps GHC select the right instance
--   based on whether 's' is a tuple. Otherwise, we'd have to use
--   TypeApplications everywhere.
class Locate s it where
  locate :: ArchTraversal -> EcsT IO (PropStore s)

instance (Locate a (IsTuple a), Locate b (IsTuple b)) => Locate (a, b) 'True where
  locate trav = liftA2 (,) (locate @a @(IsTuple a) trav) (locate @b @(IsTuple b) trav)

instance (Locate a (IsTuple a), Locate b (IsTuple b), Locate c (IsTuple c)) => Locate (a, b, c) 'True where
  locate trav = liftA3 (,,) (locate @a @(IsTuple a) trav) (locate @b @(IsTuple b) trav) (locate @c @(IsTuple c) trav)

instance (Storage c ~ PropStore c, Identify c, Component c, Pull (Layout (Storage c)) c) => Locate c 'False where
  locate = pull (Proxy @(Layout (Storage c)))




