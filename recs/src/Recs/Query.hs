{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.Query where

import Data.Hashable (Hashable)
import Data.Kind
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import GHC.Base (Any)
import GHC.Generics (Generic)
import Recs.Archetype
import Recs.Core
import Recs.Storage (Component)
import Recs.TypeInfo (identified)

{- Type Utilities
-}
type family IsTuple t where
  IsTuple (a, b) = 'True
  IsTuple (a, b, c) = 'True
  IsTuple (a, b, c, d) = 'True
  IsTuple _ = 'False

type family (l :: [Type]) ++ (m :: [Type]) :: [Type] where
  (a : as) ++ b = a : (as ++ b)
  '[] ++ b = b

type family Equals a b where
  Equals a a = 'True
  Equals _ _ = 'False

type family If c a b where
  If 'True a _ = a
  If 'False _ b = b

type family In l e where
  In (a : as) a = 'True
  In (a : as) e = In as e
  In '[] e = 'False

type family TupleToList t where
  TupleToList (a, b) = '[a, b]
  TupleToList (a, b, c) = '[a, b, c]
  TupleToList (a, b, c, d) = '[a, b, c, d]

type family ListToTuple l where
  ListToTuple '[a, b] = (a, b)
  ListToTuple '[a, b, c] = (a, b, c)
  ListToTuple '[a, b, c, d] = (a, b, c, d)
  ListToTuple '[a] = a

-- TupleToList a = '[a]
type family Map f l where
  Map f (a : as) = f a : Map f as
  Map _ '[] = '[]

type family IsList l where
  IsList (a : _) = 'True
  IsList '[] = 'True
  IsList _ = 'False

type family Or a b where
  Or 'True 'False = 'True
  Or 'False 'True = 'True
  Or 'True 'True = 'True
  Or 'False 'False = 'False

{- Query Operators
-}
data Nab c

data Stow c

data Tag c

data With c

data Not c

type Without c = Not (With c)

data a |&| b

infixr 4 |||

data a ||| b

{- Reified Query Data
-}

-- | Subject of a query like 'OpNab'.
data QuerySubject
  = -- | Component is optional. Query will not fail if missing.
    SubMaybe !TypeId
  | -- | Component is mandatory. Query will fail if missing.
    SubBase !TypeId
  deriving (Generic, Eq, Show)

instance Hashable QuerySubject

-- TODO Allow bundled subjects
data QueryOperator
  = -- | Archetype has this component for read access.
    OpNab !QuerySubject
  | -- | Archetype has this component for write access.
    OpStow !QuerySubject
  | -- | Archetype must have this component.
    OpWith !TypeId
  | -- | Archetype mustn't satisfy this query.
    OpNot !QueryOperator
  | -- | Archetype must satisfy both queries.
    OpAnd !QueryOperator !QueryOperator
  | -- | Archetype must satisfy either query.
    OpOr !QueryOperator !QueryOperator
  deriving (Generic, Eq, Show)

instance Hashable QueryOperator

-- | State of a query iterator.
data QueryTraversal = MkQueryTraversal
  { currArchId :: ArchId
  , currArch :: Archetype
  , matched :: V.Vector Any
  , matchedOk :: VU.Vector Bool
  }
  deriving (Generic)

newtype SomeQuery = MkSomeQuery
  { unSomeQuery :: V.Vector QueryTraversal
  }
  deriving (Generic)

-- | A query iterator.
data QueryHandle q = MkQueryHandle
  { entity :: (EntityId, Int)
  -- ^ Index of current entity and its ID.
  , trav :: QueryTraversal
  -- ^ Points to information about the currently iterated archetype.
  }
  deriving (Generic)

newtype Global c = MkGlobal
  { unGlobal :: c
  }

newtype Local c = MkLocal
  { unLocal :: c
  }

{- Access Control -}

-- | Optional query wrapper.
data Opt c

type family QueryItems' a b where
  QueryItems' a (b ||| bs) = '[Opt a] ++ QueryItems (b ||| bs)
  QueryItems' a b = '[Opt a] ++ '[Opt (QueryItems b)]

{- | Used to check System-local access permissions, i.e. when using 'nab' or similar
   'QueryHandle' functions.
-}
type family QueryItems q :: [Type] where
  QueryItems (a |&| b) = QueryItems a ++ QueryItems b
  QueryItems (a ||| b) = QueryItems' (QueryItems a) b
  QueryItems (Nab a) = '[Nab a]
  QueryItems (Stow a) = '[Stow a]
  QueryItems (Tag a) = '[Tag a]
  QueryItems (With a) = '[With a]

{- | Desugar a type-level query into an isomorphic 'QueryOperator', which can be used to
   match relevant archetypes.
-}
class DesugarQuery m q where
  desugarQuery :: m QueryOperator

instance (Monad m, DesugarQuery m a, DesugarQuery m b) => DesugarQuery m (a |&| b) where
  desugarQuery = OpAnd <$> desugarQuery @_ @a <*> desugarQuery @_ @b

instance (Monad m, DesugarQuery m a, DesugarQuery m b) => DesugarQuery m (a ||| b) where
  desugarQuery = OpOr <$> desugarQuery @_ @a <*> desugarQuery @_ @b

instance (Monad m, DesugarQuery m q) => DesugarQuery m (Not q) where
  desugarQuery = OpNot <$> desugarQuery @_ @q

instance Component m c => DesugarQuery m (Maybe (Nab c)) where
  desugarQuery = OpNab . SubMaybe <$> identified @_ @c

instance Component m c => DesugarQuery m (Maybe (Stow c)) where
  desugarQuery = OpStow . SubMaybe <$> identified @_ @c

instance Component m c => DesugarQuery m (Nab c) where
  desugarQuery = OpNab . SubBase <$> identified @_ @c

instance Component m c => DesugarQuery m (Stow c) where
  desugarQuery = OpStow . SubBase <$> identified @_ @c

instance Component m c => DesugarQuery m (With c) where
  desugarQuery = OpWith <$> identified @_ @c

class Queryable m q where
  query :: m q
