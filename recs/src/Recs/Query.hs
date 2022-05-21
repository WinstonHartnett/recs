{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.Query where

import Control.Applicative ((<|>))
import Data.Hashable (Hashable)
import Data.IntMap.Strict qualified as IM
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed qualified as VU
import Effectful (Eff, runPureEff)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.State.Static.Local (get)
import GHC.Base (Any)
import GHC.Generics (Generic)
import Recs.Archetype
import Recs.TypeInfo (identified, pendingTypeId)
import Recs.Types
import Unsafe.Coerce (unsafeCoerce)
import Data.Word (Word16)
import Recs.Utils

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
  | -- | Archetype must not satisfy this query.
    OpNot !QueryOperator
  | -- | Archetype must satisfy both queries.
    OpAnd !QueryOperator !QueryOperator
  | -- | Archetype must satisfy either query.
    OpOr !QueryOperator !QueryOperator
  deriving (Generic, Eq, Show)

instance Hashable QueryOperator

-- | State of a query iterator.
data QueryTraversal = MkQueryTraversal
  { currArchId :: !ArchId
  , currArch :: !Archetype
  , matched :: !(VU.Vector Word16)
  -- ^ Vector mapping 'TypeId' to indices into the 'currArch' stores
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

newtype Query q = MkQuery {unQuery :: V.Vector (QueryHandle q)}

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
class DesugarQuery q where
  desugarQuery :: Ecs es => Eff es QueryOperator

instance (DesugarQuery a, DesugarQuery b) => DesugarQuery (a |&| b) where
  desugarQuery = OpAnd <$> desugarQuery @a <*> desugarQuery @b

instance (DesugarQuery a, DesugarQuery b) => DesugarQuery (a ||| b) where
  desugarQuery = OpOr <$> desugarQuery @a <*> desugarQuery @b

instance DesugarQuery q => DesugarQuery (Not q) where
  desugarQuery = OpNot <$> desugarQuery @q

instance Component c => DesugarQuery (Maybe (Nab c)) where
  desugarQuery = OpNab . SubMaybe <$> identified @c

instance Component c => DesugarQuery (Maybe (Stow c)) where
  desugarQuery = OpStow . SubMaybe <$> identified @c

instance Component c => DesugarQuery (Nab c) where
  desugarQuery = OpNab . SubBase <$> identified @c

instance Component c => DesugarQuery (Stow c) where
  desugarQuery = OpStow . SubBase <$> identified @c

instance Component c => DesugarQuery (With c) where
  desugarQuery = OpWith <$> identified @c

class Queryable q where
  query :: Ecs es => Eff es q

instance DesugarQuery q => Queryable (Query q) where
  query = do
    queryResult <- desugarQuery @q >>= runQuery
    MkQuery <$> V.mapM undefined (unSomeQuery queryResult)

instance Component c => Queryable (Global c) where
  query = do
    ecs <- get @World
    globalId <- identified @c
    unsafeCoerce @Any @(Global c) <$> VR.read ecs.globals.unGlobals (from globalId)

-- | Get the return type of a function.
type family Returns s where
  Returns (a -> b) = Returns b
  Returns (Eff es a) = Eff es a

class Queried s where
  queried :: Ecs es => s -> Eff es (Returns s)

instance (Queryable p, Queried s) => Queried (p -> s) where
  queried s = (s <$> query @p) >>= queried

instance System es => Queried (Eff es a) where
  queried = pure

----------------------------------------------------------------------------------------------------
-- -- | Whether an archetype is relevant given a query.
-- --   'Nothing' implies a query has failed. 'Just a' implies that a query has succeeded,
-- --   mapping a type to an archetype's matching component store.
type QueryRelevant = Reader Archetype

unsafeGetQueryStore :: forall c q es. (Component c, Ecs es) => QueryHandle q -> Eff es (Layout c)
unsafeGetQueryStore qh = do
  ident <- identified @c
  undefined
  -- pure $ unsafeCoerce @Any @(Layout c) $ qh.trav.matched VG.! from ident

-- unsafeModifyQueryStore :: forall c q es. (Component c, Ecs es) => QueryHandle q -> (Layout c -> Eff es (Layout c)) -> Eff es ()
-- unsafeModifyQueryStore qh f = do
--   ident <- identified @c
--   let target = unsafeCoerce @Any @(Layout c) $ qh.trav.matched VG.! from ident
--   f target

basicHas :: QuerySubject -> Eff '[QueryRelevant] (Maybe (IM.IntMap Any))
basicHas qs = do
  arch <- ask @Archetype
  let targetStore = getStore' arch
  pure case qs of
    SubMaybe tId -> Just $ fromMaybe (IM.empty) $ IM.singleton (from tId) <$> targetStore tId
    SubBase tId -> IM.singleton (from tId) <$> targetStore tId

processOperator :: QueryOperator -> Eff '[QueryRelevant] (Maybe (IM.IntMap Any))
processOperator = \case
  OpNab qs -> basicHas qs
  OpStow qs -> basicHas qs
  OpWith tId -> basicHas (SubBase tId) >> pure (Just $ IM.empty)
  OpNot qn -> maybe (Just $ IM.empty) (const Nothing) <$> processOperator qn
  OpAnd a b -> do
    aRes <- processOperator a
    bRes <- processOperator b
    pure $ IM.union <$> aRes <*> bRes
  OpOr a b -> (<|>) <$> processOperator a <*> processOperator b

runQueryOperator :: Archetype -> QueryOperator -> Maybe (IM.IntMap Any)
runQueryOperator arch = runPureEff . runReader arch . processOperator

runQueryOn :: Ecs es => QueryOperator -> V.Vector Archetype -> Eff es SomeQuery
runQueryOn qo as = do
  let filterArch (aId, arch) = do
        nextTId <- pendingTypeId
        pure do
          queryRes <- runQueryOperator arch qo
          let queryEntries = IM.toList queryRes
              matched' = VG.fromList $ map (tryFrom' . fst) queryEntries
          pure
            MkQueryTraversal
              { currArchId = aId
              , currArch = arch
              -- , matched = matched'
              , matched = matched'
              }
  traversals <- VG.mapMaybeM filterArch . VG.map (over _1 (MkArchId . from)) . VG.indexed $ as
  pure $ MkSomeQuery traversals

cacheQuery :: QueryOperator -> SomeQuery -> Eff es ()
cacheQuery = undefined

runQuery :: Ecs es => QueryOperator -> Eff es SomeQuery
runQuery qo = runQueryOn qo =<< VR.freeze . view (#archetypes . #archetypes) =<< get @World

runAndCacheQuery :: Ecs es => QueryOperator -> Eff es SomeQuery
runAndCacheQuery qo = do
  q <- runQuery qo
  cacheQuery qo q
  pure q
