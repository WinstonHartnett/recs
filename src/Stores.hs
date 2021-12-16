{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Stores where

import Core
import qualified Data.Vector.Growable as VR
import Data.IORef (IORef)
import Control.Lens hiding (index)
import Data.Generics.Labels
import Unsafe.Coerce (unsafeCoerce)
import Control.Applicative (liftA2, liftA3)
import Data.Coerce (coerce)
import GHC.Generics (Generic)
import GHC.TypeLits
import TypeLevel
import qualified Data.Vector.Unboxed as VU
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V

---------------------------------------------------------------------------------
-- Classes
---------------------------------------------------------------------------------

-- | If a store's 'Layout' is 'Archetypal', then it will be stored in the
--   archetype graph.
--   If it is 'Flat', then it will be stored once in the global flat data field
--   of the ECS.
data StoreLayout
  = Archetypal
  | Flat
  deriving Generic

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

instance {-# OVERLAPPABLE #-} (Identify c, Component c) => Pull Archetypal c where
  pull _ trav =
    let storeIdx = (trav ^. #matched) VU.! identify (Proxy @c)
        pulled = (trav ^. #currArch . #components) V.! storeIdx
      in pure . unsafeCoerce @_ @(Storage c) $ pulled

instance (Identify c, Component c) => Pull Flat c where
  -- pull _ _ = unsafeCoerce @_ @(Storage a) <$> pullFlat (identify $ Proxy @a)
  pull _ _ = undefined

type family PropStore s where
  PropStore (a, b) = (PropStore a, PropStore b)
  PropStore (a, b, c) = (PropStore a, PropStore b, PropStore c)
  PropStore (a, b, c, d) = (PropStore a, PropStore b, PropStore c, PropStore d)
  PropStore (a, b, c, d, e) = (PropStore a, PropStore b, PropStore c, PropStore d, PropStore e)
  PropStore a = Storage a

type family Layout s :: StoreLayout

-- | 'Locate' is used to pull a store for each member of a tuple. For example:
--
--   @
--      (a :: Storage Velocity, (b :: Storage Time, c :: Storage Whatever)) <- locate trav
--   @
--
--   This is then passed to 'Get', which indexes into each store:
--
--   @
--      (x :: (Storage Velocity, (Storage Time, Storage Whatever)) <- locate trav
--      (MkVelocity v, (MkTime t, MkWhatever)) <- index x (myEntityId)
--   @
class Locate s isTuple where
  locate :: ArchTraversal -> EcsT IO (PropStore s)

instance (Locate a (IsTuple a), Locate b (IsTuple b)) => Locate (a, b) 'True where
  locate trav = liftA2 (,) (locate @a @(IsTuple a) trav) (locate @b @(IsTuple b) trav)

instance (Locate a (IsTuple a), Locate b (IsTuple b), Locate c (IsTuple c)) => Locate (a, b, c) 'True where
  locate trav = liftA3 (,,) (locate @a @(IsTuple a) trav) (locate @b @(IsTuple b) trav) (locate @c @(IsTuple c) trav)

instance (Storage c ~ PropStore c, Identify c, Component c, Pull (Layout (Storage c)) c) => Locate c 'False where
  locate = pull (Proxy @(Layout (Storage c)))

---------------------------------------------------------------------------------
-- Default Stores
---------------------------------------------------------------------------------

newtype UMap c = MkUMap { unUMap :: VR.GrowableUnboxedIOVector c}
  deriving Generic

type instance Layout (UMap c) = Archetypal
type instance Elem (UMap c) = c

instance Component c => Get m (UMap c) where

---------------------------------------------------------------------------------
newtype BMap c = MkBMap { unBMap :: VR.GrowableIOVector c }
  deriving Generic

instance Component c => Get m (BMap c)

type instance Layout (BMap c) = Archetypal
type instance Elem (BMap c) = c

---------------------------------------------------------------------------------
newtype Global c = MkGlobal (IORef c)
  deriving Generic

type instance Layout (Global c) = Flat
type instance Elem (Global c) = c

---------------------------------------------------------------------------------

newtype MaybeStore s = MkMaybeStore { unMaybeStore :: Maybe s }
  deriving Generic

type instance Elem (MaybeStore s) = Maybe (Elem s)
type instance Layout (MaybeStore s) = Archetypal

instance (Identify c, Component c) => Identify (Maybe c) where
  type Identified (Maybe c) = Identified c

instance Component c => Component (Maybe c) where
  type Storage (Maybe c) = MaybeStore (Storage c)

instance Pull Archetypal c => Pull Archetypal (Maybe c) where
  pull _ trav = do
    let tId = identify $ Proxy @c
        archHas = VU.elem tId (unTypeId $ trav ^. #currArch . #types)
    undefined

instance Get IO s => Get IO (MaybeStore s) where
  index (MkMaybeStore s) idx = traverse (`index` idx) s

---------------------------------------------------------------------------------


