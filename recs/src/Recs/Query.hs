{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.Query where

import Data.Kind
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Growable qualified as VR
import Effectful
import Effectful.Prim (Prim)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.State.Static.Local (get, modify)
import GHC.Base (Any)
import Recs.Archetype
import Recs.TypeInfo (identified)
import Recs.Types
import Recs.Utils
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.HashMap.Strict as HMS

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

{- | Whether an archetype is relevant given a query.
   'Nothing' implies a query has failed. 'Just a' implies that a query has succeeded,
   mapping a type to an archetype's matching component store.
-}
type QueryRelevant es = (Reader Archetype :> es, Prim :> es)

unsafeGetQueryStore :: forall c q es. (Component c, Ecs es) => QueryHandle q -> Eff es (Layout c)
unsafeGetQueryStore qh = unsafeGetStore' @c qh.trav.currArch

unsafeModifyQueryStore :: forall c q es. (Component c, Ecs es) => QueryHandle q -> (Layout c -> Eff es ()) -> Eff es ()
unsafeModifyQueryStore qh f = unsafeGetQueryStore @c qh >>= f

basicHas :: QueryRelevant es => QuerySubject -> Eff es Bool
basicHas qs = do
  arch <- ask @Archetype
  case qs of
    SubMaybe _ -> pure True
    SubBase tId -> hasStore tId arch

processOperator :: QueryRelevant es => QueryOperator -> Eff es Bool
processOperator = \case
  OpNab qs -> basicHas qs
  OpStow qs -> basicHas qs
  OpWith tId -> basicHas $ SubBase tId
  OpNot qn -> processOperator qn
  OpAnd a b -> (&&) <$> processOperator a <*> processOperator b
  OpOr a b -> (||) <$> processOperator a <*> processOperator b

runQueryOperator :: Prim :> es => Archetype -> QueryOperator -> Eff es Bool
runQueryOperator arch = runReader arch . processOperator

runQueryOn :: Prim :> es => QueryOperator -> V.Vector Archetype -> Eff es SomeQuery
runQueryOn qo as = do
  let filterArch (aId, arch) = do
        _ <- runQueryOperator arch qo
        pure $ Just $ MkQueryTraversal{currArchId = aId, currArch = arch}
  traversals <- VG.mapMaybeM filterArch . VG.map (over _1 (MkArchId . from)) . VG.indexed $ as
  pure $ MkSomeQuery traversals

cacheQuery :: Ecs es => QueryOperator -> SomeQuery -> Eff es ()
cacheQuery qo sq = modify @World (over (#queryInfo . #cachedQueries) (HMS.insert qo sq))

cacheQueries :: Ecs es => V.Vector (QueryOperator, SomeQuery) -> Eff es ()
cacheQueries qs = VG.forM_ qs \(qo, sq) -> cacheQuery qo sq

runQuery :: Ecs es => QueryOperator -> Eff es SomeQuery
runQuery qo = runQueryOn qo =<< VR.freeze . view (#archetypes . #archetypes) =<< get @World

runQueries :: Ecs es => V.Vector QueryOperator -> Eff es (V.Vector SomeQuery)
runQueries qo = do
  arches <- VR.freeze . view (#archetypes . #archetypes) =<< get @World
  VG.forM qo (`runQueryOn` arches)

runAndCacheQuery :: Ecs es => QueryOperator -> Eff es SomeQuery
runAndCacheQuery qo = do
  q <- runQuery qo
  cacheQuery qo q
  pure q
