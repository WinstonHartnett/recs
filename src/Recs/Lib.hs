
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Recs.Lib where

import           Control.Applicative          (liftA2, (<|>))
import           Control.Lens                 hiding (from)
import           Control.Monad.Catch          (MonadCatch,MonadMask,MonadThrow)
import           Control.Monad.Primitive      (PrimMonad(..))
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

import           Data.Coerce
import           Data.Default
import           Data.Either                  (fromRight)
import           Data.Generics.Labels
import qualified Data.HashMap.Internal        as HM
import           Data.HashMap.Internal        (Hash)
import qualified Data.HashMap.Strict          as HM
import           Data.Hashable
import qualified Data.IntMap                  as IM
import qualified Data.IntSet                  as IS
import           Data.IORef
import qualified Data.Set                     as S
import qualified Data.Sequence                as SQ
import           Data.Kind
import           Data.Maybe                   (fromJust,fromMaybe,isJust)
import           Data.Proxy                   (Proxy(..))
import qualified Data.Text                    as T
import           Data.Typeable                (Typeable,typeRep,typeRepFingerprint)
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Generic.Mutable  as VGM
import qualified Data.Vector.Growable         as VR
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Algorithms.Search as V
import qualified Data.Vector.Algorithms.Heap as V
import qualified Data.Vector.Unboxed          as VU
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as VUM
import           Data.Word

import           GHC.Base                     (Any,RealWorld, IO(..))
import           GHC.Fingerprint              (Fingerprint(..))
import           GHC.Generics                 (Generic)
import           GHC.TypeLits
import           GHC.Stack                    (HasCallStack)

import           Unsafe.Coerce

import           Witch                        hiding (over)

untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM p f a = do
  cond <- a >>= p
  if cond
     then a
     else untilM p f (a >>= f)

instance (VU.Unbox a, Hashable a) => Hashable (VU.Vector a) where
  hashWithSalt i = hashWithSalt i . VG.toList

-----------------------------------------------------------------------------------------
-- Typelevel Programming
-----------------------------------------------------------------------------------------
type family IsTuple t where
  IsTuple (a, b) = True
  IsTuple (a, b, c) = True
  IsTuple (a, b, c, d) = True
  IsTuple _ = False

type family (l :: [Type]) ++  (m :: [Type]) :: [Type] where
  (a : as) ++ b = a : (as ++ b)
  '[] ++ b = b

type family Equals a b where
  Equals a a = True
  Equals _ _ = False

type family If c a b where
  If True a _ = a
  If False _ b = b

type family In l e where
  In (a : as) a = True
  In (a : as) e = In as e
  In '[] e = False

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
  IsList (a : _) = True
  IsList '[] = True
  IsList _ = False

type family Or a b where
  Or True False = True
  Or False True = True
  Or True True = True
  Or False False = False

-----------------------------------------------------------------------------------------
-- Core Data
-----------------------------------------------------------------------------------------
newtype TypeId = MkTypeId Word16
  deriving (Generic,Eq,Ord,Show)

instance From TypeId Int where
  from (MkTypeId w) = from w

instance TryFrom Int TypeId where
  tryFrom i = case tryFrom i of
    Left tfe -> Left $ coerce tfe
    Right a  -> Right $ MkTypeId a

instance Hashable TypeId

newtype ArchId = MkArchId Word32
  deriving (Generic,Eq,Ord,Show)

instance From ArchId Int where
  from (MkArchId a) = fromIntegral a

instance Hashable ArchId

newtype EntityId = MkEntityId Word32
  deriving (Generic,Eq,Ord,Show)

instance From EntityId Int where
  from (MkEntityId a) = fromIntegral a

instance Hashable EntityId

derivingUnbox "EntityId" [t|EntityId -> Word32|] [|\(MkEntityId i) -> i|] [|MkEntityId|]

derivingUnbox "ArchId" [t|ArchId -> Word32|] [|\(MkArchId i) -> i|] [|MkArchId|]

derivingUnbox "TypeId" [t|TypeId -> Word16|] [|\(MkTypeId w) -> w|] [|MkTypeId|]

-----------------------------------------------------------------------------------------
-- Query Operators
-----------------------------------------------------------------------------------------
data Nab c

data Stow c

data Tag c

data With c

data Not c

type Without c = Not (With c)

data a |&| b

infixr 4 |||

data a ||| b

-----------------------------------------------------------------------------------------
-- | Subject of a query like 'OpNab'.
data QuerySubject =
      -- | Component is optional. Query will not fail if missing.
      SubMaybe !TypeId
      -- | Component is mandatory. Query will fail if missing.
    | SubBase !TypeId
    deriving (Generic, Eq, Show)

instance Hashable QuerySubject

-- TODO Allow bundled subjects
data QueryOperator =
      -- | Archetype has this component for read access.
      OpNab  !QuerySubject
      -- | Archetype has this component for write access.
    | OpStow !QuerySubject
      -- | Archetype must have this component.
    | OpWith !TypeId
      -- | Archetype mustn't satisfy this query.
    | OpNot  !QueryOperator
      -- | Archetype must satisfy both queries.
    | OpAnd  !QueryOperator !QueryOperator
      -- | Archetype must satisfy either query.
    | OpOr   !QueryOperator !QueryOperator
    deriving (Generic, Eq, Show)

instance Hashable QueryOperator

-----------------------------------------------------------------------------------------
-- | State of a query iterator.
data QueryTraversal =
  MkQueryTraversal
  { currArchId :: ArchId
  , currArch   :: Arch
  , matched    :: V.Vector Any
  , matchedOk  :: VU.Vector Bool
  }
  deriving Generic

newtype SomeQuery =
  MkSomeQuery
  { unSomeQuery :: V.Vector QueryTraversal
  }
  deriving Generic

-- | A query iterator.
data QueryHandle q =
  MkQueryHandle
  { -- | Index of current entity.
    entity :: Int
    -- | Points to information about the currently iterated archetype.
  , trav   :: QueryTraversal
  }
  deriving Generic

newtype Query q =
  MkQuery
  { unQuery :: V.Vector (QueryHandle q)
  }
  deriving Generic

newtype Global c =
  MkGlobal
  { unGlobal :: c
  }
  deriving Generic

data Local c =
  MkLocal
  { unLocal :: c
  }
  deriving Generic

-----------------------------------------------------------------------------------------
-- Access Control
-----------------------------------------------------------------------------------------

-- | Optional query wrapper.
data Opt c

type family QueryItems' a b where
  QueryItems' a (b ||| bs) = '[Opt a] ++ QueryItems (b ||| bs)
  QueryItems' a b = '[Opt a] ++ '[Opt (QueryItems b)]

-- | Used to check System-local access permissions, i.e. when using 'nab' or similar
--   'QueryHandle' functions.
type family QueryItems q :: [Type] where
  QueryItems (a |&| b) = QueryItems a ++ QueryItems b
  QueryItems (a ||| b) = QueryItems' (QueryItems a) b
  QueryItems (Nab a) = '[Nab a]
  QueryItems (Stow a) = '[Stow a]
  QueryItems (Tag a) = '[Tag a ]
  QueryItems (With a) = '[With a]

-----------------------------------------------------------------------------------------
-- Archetypes
-----------------------------------------------------------------------------------------

data Edge =
  MkEdge
  { add    :: !(Maybe ArchId)
  , remove :: !(Maybe ArchId)
  }
  deriving (Generic,Show)

data Arch =
  MkArch
  { components :: V.Vector Any
  , entities   :: VR.GrowableUnboxedIOVector (Bool, EntityId)
  , types      :: VU.Vector TypeId
  , edges      :: VR.GrowableUnboxedIOVector Edge
  }
  deriving Generic

--------------------------------------------------------------------------------
data ArchSearch
  = Add (VU.Vector TypeId)
  | Remove (VU.Vector TypeId)

data ArchRecord =
  MkArchRecord
  { archId :: ArchId
  , row    :: Int
  }
  deriving (Generic,Show)

--------------------------------------------------------------------------------
data EntityInfo =
  MkEntityInfo
  { entityMap :: IM.IntMap ArchRecord
    -- TODO Change to PVar
  , entityCtr :: IORef EntityId
  }
  deriving Generic

instance {-# OVERLAPPING #-} Default (IO EntityInfo) where
  def = do
    e <- newIORef (MkEntityId 0)
    pure $ MkEntityInfo
      { entityMap = IM.empty
      , entityCtr = e
      }

data TypeInfo =
  MkTypeInfo
  { -- | Map a GHC Fingerprint to a unique type identifier.
    types    :: HM.HashMap Fingerprint TypeId
    -- | Next type identifier.
    -- TODO Change to PVar
  , typeCtr  :: IORef TypeId
    -- | Constructors for type-specific stores.
    --   These are needed to create new archetypes with unknown runtime types.
    --   By default, all types point to 'GrowableIOVector'.
  , typeCons :: V.Vector (Ecs Any)
  }
  deriving (Generic)

instance {-# OVERLAPPING #-} Default (IO TypeInfo) where
  def = do
    t <- newIORef (MkTypeId 0)
    pure $ MkTypeInfo
      { types   = HM.empty
      , typeCtr = t
      , typeCons = V.empty
      }

-- | A cached query entry.
data QueryEntry = MkQueryEntry
  { already :: HM.HashMap ArchId Int
  , result  :: SomeQuery
  }
  deriving Generic

instance Default QueryEntry where
  def = MkQueryEntry HM.empty (MkSomeQuery [])

data World =
  MkWorld
  { archetypes  :: VR.GrowableIOVector Arch
  , globals     :: VR.GrowableIOVector Any
  , types       :: TypeInfo
  , entities    :: EntityInfo
  , generations :: VR.GrowableIOVector (VR.GrowableUnboxedIOVector ArchId)
  , queries     :: HM.HashMap QueryOperator QueryEntry
  }
  deriving Generic

instance {-# OVERLAPPING #-} Default (IO World) where
  def = do
    arc <- VR.new
    glo <- VR.new
    t <- def
    e <- def
    gen <- VR.new
    pure $ MkWorld
        { archetypes = arc
        , globals = glo
        , types = t
        , entities = e
        , generations = gen
        , queries = HM.empty
        }

newtype Ecs a =
  MkEcs
  { unEcs :: StateT World IO a
  }
  deriving (Functor,Applicative,Monad,MonadState World,MonadIO,MonadThrow,MonadCatch
           ,MonadMask)

instance PrimMonad Ecs where
  type PrimState Ecs = RealWorld
  primitive = MkEcs . lift . IO

-----------------------------------------------------------------------------------------
newtype Command = MkCommand { unCommand :: Ecs () }
  deriving Generic

newtype Commands = MkCommands { unCommands :: VR.GrowableIOVector Command }
  deriving Generic

newtype SystemState = MkSystemState
  { commands :: Maybe Commands
  }
  deriving Generic

instance Default SystemState where
  def = MkSystemState { commands = Nothing }

newtype SystemT m a = MkSystemT
  { unSystem :: StateT SystemState m a
  }
  deriving (Generic, Functor, Applicative, Monad, MonadIO, MonadState SystemState, MonadTrans, MonadThrow)

runSystem :: System a -> Ecs (a, SystemState)
runSystem sys = runStateT (unSystem sys) def

instance PrimMonad m => PrimMonad (SystemT m) where
  type PrimState (SystemT m) = PrimState m
  primitive = MkSystemT . primitive

type System a = SystemT Ecs a

-----------------------------------------------------------------------------------------
derivingUnbox "Edge" [t|Edge -> (ArchId, ArchId)|] [|\(MkEdge a r)
  -> (fromMaybe (MkArchId (-1)) a, fromMaybe (MkArchId (-1)) r)|] [|\(a, r)
  -> let convertArchId i =
           if i == MkArchId (-1)
             then Nothing
             else Just i
     in MkEdge (convertArchId a) (convertArchId r)|]

-- TODO This sucks o-wise
typeIdDiff :: VU.Vector TypeId -> VU.Vector TypeId -> VU.Vector TypeId
typeIdDiff a b =
  let buildISet = IS.fromDistinctAscList . VG.toList . VG.map from
      (a', b') = (buildISet a, buildISet b)
    in  VG.map (MkTypeId . fromIntegral)
      . VG.fromList
      . IS.toList
      $ IS.union a' b' `IS.difference` IS.intersection a' b'

-- | Provide a unique hash for each type.
class Typeable t => Identify t where
  identify :: (Hash, Fingerprint)
  default identify :: (Hash, Fingerprint)

  --   TODO Generate these w/ TH
  identify =
    case typeRepFingerprint . typeRep $ Proxy @t of
      f@(Fingerprint h _) -> (fromRight (error "Word size mismatch") $ tryFrom h, f)

instance {-# OVERLAPPABLE #-} Typeable t => Identify t

class Storage a where
  type Elem a
  type Idx a
  getIdxS :: QueryTraversal -> Int -> Ecs (Idx a)
  initS :: Ecs a
  insertS :: a -> Elem a -> Ecs ()
  removeS :: a -> Idx a -> Ecs ()
  lookupS :: a -> Idx a -> Ecs (Elem a)
  modifyS :: a -> Idx a -> Elem a -> Ecs ()

-- | 'Component' specifies which storage structure to use for each type.
--
--   Storage defaults to 'Unboxed' for performance reasons. You must derive
--   'Component via Boxed' if you want Boxed storage.
class (Storage (Layout c), Identify c) => Component (c :: Type) where
  type Layout c

reserveAtomic :: Lens' World (IORef c) -> (c -> (c, b)) -> Ecs b
reserveAtomic l f = liftIO . (`atomicModifyIORef` f) . view l =<< get

reserveTypeId =
  reserveAtomic
    (#types . #typeCtr)
    (\t@(MkTypeId i) -> (MkTypeId $ i + 1, t))

reserveEntityId =
  reserveAtomic
    (#entities . #entityCtr)
    (\e@(MkEntityId i) -> (MkEntityId $ i + 1, e))

-- | Retrieve this type's ID from the global type registry, adding any necessary
--   information to the type registry.
identified :: forall c. Component c => Ecs TypeId
identified = do
  ecs <- get
  case uncurry HM.lookup' (identify @c) (ecs ^. (#types . #types)) of
    Just tId -> pure tId
    Nothing -> do
      nextTId <- reserveTypeId
      let (h, f) = identify @c
          updateTInfo =
            let addTCon tCons = VG.snoc tCons (unsafeCoerce <$> initS @(Layout c))
             in over #types (over #typeCons addTCon
                              . over #types (HM.insert' h f nextTId))
          pushNewEdges = VR.fromGrowable (ecs ^. #archetypes) >>= VM.mapM_ (\a -> do
                          VR.push (a ^. #edges) (MkEdge Nothing Nothing))
          pushGlobals = VR.push (ecs ^. #globals) (unsafeCoerce . error $ "Tried to access missing global '" <> show (typeRep $ Proxy @c) <> "'.")
      modify updateTInfo >> pushNewEdges >> pushGlobals >> pure nextTId

instance Storage (VR.GrowableIOVector c) where
  type Elem (VR.GrowableIOVector c) = c
  type Idx (VR.GrowableIOVector c) = Int
  getIdxS _ i = pure i
  initS = VR.new
  insertS = VR.push
  removeS _ _ = pure ()
  lookupS = VR.read
  modifyS = VR.write

instance VU.Unbox c => Storage (VR.GrowableUnboxedIOVector c) where
  type Elem (VR.GrowableUnboxedIOVector c) = c
  type Idx (VR.GrowableUnboxedIOVector c) = Int
  getIdxS _ i = pure i
  initS = VR.new
  insertS = VR.push
  removeS _ _ = pure ()
  lookupS = VR.read
  modifyS = VR.write

type Boxed c = VR.GrowableIOVector c

type Unboxed c = VR.GrowableUnboxedIOVector c

instance {-# OVERLAPPABLE #-} (VU.Unbox c, Identify c) => Component c where
  type Layout c = Unboxed c

insertGlobal :: forall g. Component g => g -> Ecs ()
insertGlobal g = do
  ecs <- get
  globalId <- identified @g
  VR.write (ecs ^. #globals) (from globalId) (unsafeCoerce @g @Any g)

--------------------------------------------------------------------------------
-- | Retrieves a system argument. For queries, this means either loading results
--   from cache or executing a query and caching it.
class Queryable q where
  -- FIXME Redo this whole setup to properly use System
  query :: Ecs q

tryFrom' :: TryFrom a b => a -> b
tryFrom' = fromRight undefined . tryFrom

instance DesugarQuery q => Queryable (Query q) where
  query = do
    res <- desugarQuery @q >>= runQuery
    pure . MkQuery . V.map (MkQueryHandle 0) . unSomeQuery $ res

-- TODO loosen superclass bound
instance Component c => Queryable (Global c) where
  query = do
    ecs <- get
    globalId <- identified @c
    unsafeCoerce @_ @(Global c) <$> VR.read (ecs ^. #globals) (from globalId)

instance Queryable (Local c) where
  query = undefined

instance Queryable Commands where
  query = do
    v <- VR.new
    pure $ MkCommands v

-----------------------------------------------------------------------------------------
-- | Get the return type of a function.
type family Returns s where
  Returns (a -> b) = Returns b
  -- Returns (System a) = (a, Commands)
  Returns (System a) = System a

-- | Saturate a system's arguments as needed.
class Queried s where
  queried :: s -> Ecs (Returns s)

instance (Queryable p, Queried s) => Queried (p -> s) where
  queried s = (s <$> query @p) >>= queried

instance Queried (System a) where
  queried = pure . id

-----------------------------------------------------------------------------------------
-- TODO For now just one component
nab :: forall c q qi. (qi ~ QueryItems q, In qi (Nab c) ~ True, Component c) => QueryHandle q -> System c
nab qh = do
  targetStore <- lift $ getStore @c qh
  lift $ lookupS targetStore (qh ^. #entity)

-----------------------------------------------------------------------------------------
stow :: forall c q qi. (qi ~ QueryItems q, In qi (Stow c) ~ True, Component c) => QueryHandle q -> c -> System ()
stow qh c = do
  targetStore <- lift $ getStore @c qh
  lift $ modifyS targetStore (qh ^. #entity) c
-----------------------------------------------------------------------------------------

tag = undefined

untag = undefined

findNextAvailEntity :: VUM.IOVector (Bool, EntityId) -> Ecs (Maybe Int)
findNextAvailEntity vum = loop 0
  where
    vumLen = VUM.length vum
    loop idx
      | idx >= vumLen = pure Nothing
      | otherwise = do
          (open, _) <- VUM.read vum idx
          if open
             then loop (idx + 1)
             else pure $ Just idx

data CommandBuilder = MkCommandBuilder
  { commands   :: Commands
  , components :: V.Vector (TypeId, Maybe Int -> Arch -> Ecs ())
  }
  deriving Generic

commitCommands :: Commands -> Ecs ()
commitCommands (MkCommands commands) =
  VR.fromGrowable commands >>= VM.mapM_ (\(MkCommand c) -> c)

spawn :: Commands -> System CommandBuilder
spawn c = pure (MkCommandBuilder { commands = c, components = V.empty })

tagged :: forall c. Component c => c -> CommandBuilder -> System CommandBuilder
tagged c cb = lift $ do
  ident <- identified @c
  let modF = \idx targetArch -> do
        let (Just targetIdx) = findStoreIdx ident targetArch
            targetStore = unsafeCoerce @_ @(Layout c) $ (targetArch ^. #components) VG.! targetIdx
         in maybe (insertS targetStore c) (\i -> modifyS targetStore i c) idx
  pure $ over #components (`VG.snoc` (ident, modF)) cb

finish :: CommandBuilder -> System EntityId
finish cb = do
  modify (set #commands (Just $ cb ^. #commands))
  lift $ do
    targetArch <- traverseRoot (Add . VG.convert . VG.map fst $ cb ^. #components)
    nextIdx <- findNextAvailEntity =<< VR.fromGrowable (targetArch ^. #entities)
    VG.forM_ (cb ^. #components) (\(_, f) ->
      VR.push (cb ^. #commands . #unCommands) (MkCommand $ f nextIdx targetArch))
    let archEntities = targetArch ^. #entities
    case nextIdx of
      Just idx -> do
        VR.modify archEntities (set _1 True) idx
        view _2 <$> VR.read archEntities idx
      Nothing -> do
        newEntId <- reserveEntityId
        VR.push archEntities (True, newEntId)
        pure newEntId

destroy = undefined
-----------------------------------------------------------------------------------------
-- | Desugar a type-level query into an isomorphic 'QueryOperator', which can be used to
--   match relevant archetypes.
class DesugarQuery q where
  desugarQuery :: Ecs QueryOperator

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

-----------------------------------------------------------------------------------------
-- | Whether an archetype is relevant given a query.
--   'Nothing' implies a query has failed. 'Just a' implies that a query has succeeded,
--   mapping a type to an archetype's matching component store.
type QueryRelevant = ReaderT (Arch, ArchId) (MaybeT Ecs) (IM.IntMap Any)

findStoreIdx :: TypeId -> Arch -> Maybe Int
findStoreIdx tId = VU.findIndex (== tId) . view #types

basicHas :: QuerySubject -> QueryRelevant
basicHas qs = do
  (a, _) <- ask
  let targetStore = \tId -> ((a ^. #components) VG.!) <$> findStoreIdx tId a
  case qs of
    SubMaybe tId -> pure . maybe IM.empty (IM.singleton (from tId)) $ targetStore tId
    SubBase tId -> maybe mzero (pure . IM.singleton (from tId)) $ targetStore tId

processOperator :: QueryOperator -> QueryRelevant
processOperator (OpNab qs) = basicHas qs
processOperator (OpStow qs) = basicHas qs
processOperator (OpWith tId) = basicHas (SubBase tId) >> pure IM.empty
processOperator (OpNot qo) = do
  res <- lift . lift . runMaybeT . runReaderT (processOperator qo) =<< ask
  maybe (pure IM.empty) (const mzero) res
processOperator (OpAnd a b) = IM.union <$> processOperator a <*> processOperator b
processOperator (OpOr a b) = processOperator a <|> processOperator b

runQueryOperator :: (Arch, ArchId) -> QueryOperator -> MaybeT Ecs (IM.IntMap Any)
runQueryOperator a = flip runReaderT a . processOperator
-----------------------------------------------------------------------------------------

runQueryOn :: QueryOperator -> V.Vector Arch -> Ecs SomeQuery
runQueryOn qo as = do
  ecs <- get
  let filterArch (aId, a) = runMaybeT $ do
        queryRes <- runQueryOperator (a, aId) qo
        nextTId <- liftIO $ from <$> readIORef (ecs ^. #types . #typeCtr)
        let queryEntries = IM.toList queryRes
            matched' = VG.create $ do
              v <- VGM.new nextTId
              forM_ queryEntries (\(i, a) -> VGM.write v i a)
              pure v
            matchedOk' = VG.create $ do
              v <- VGM.new nextTId
              forM_ (map fst queryEntries) (\i -> VGM.write v i True)
              pure v
        pure $ MkQueryTraversal
          { currArchId = aId
          , currArch = a
          , matched = matched'
          , matchedOk = matchedOk'
          }
  MkSomeQuery <$> (VG.mapMaybeM filterArch . VG.map (over _1 (MkArchId . fromIntegral)) . VG.indexed $ as)

cacheQuery :: QueryOperator -> SomeQuery -> Ecs ()
cacheQuery qo sq = undefined

runQuery :: QueryOperator -> Ecs SomeQuery
runQuery qo = runQueryOn qo =<< VR.freeze . view #archetypes =<< get

runAndCacheQuery :: QueryOperator -> Ecs SomeQuery
runAndCacheQuery qo = do
  q <- runQuery qo
  cacheQuery qo q
  pure q

getStore :: forall c q. Component c => QueryHandle q -> Ecs (Layout c)
getStore qh = do
  ident <- identified @c
  let currArch = qh ^. #trav . #currArch
      targetStore = (currArch ^. #components) VG.! fromJust (findStoreIdx ident currArch)
  pure $ unsafeCoerce @_ @(Layout c) $ targetStore

traverseArch :: Arch -> ArchSearch -> Ecs Arch
traverseArch root as = do
  ecs <- get
  let (tIds, edgeLens) = case as of
        Add t    -> (t, #add)
        Remove t -> (t, #remove)
      tLen = VG.length tIds
      loop idx root'
        | idx >= tLen = pure root'
        | otherwise = do
            let nextTId = tIds VG.! idx
            targetEdge <- view edgeLens <$> (root' ^. #edges) `VR.read` from nextTId
            nextRoot <- case targetEdge of
              Just r  -> (ecs ^. #archetypes) `VR.read` from r
              Nothing -> view _2 <$> createArch (VG.modify V.sort $ root ^. #types VG.++ VG.take (idx + 1) tIds)
            loop (idx + 1) nextRoot
  loop 0 root

traverseRoot :: ArchSearch -> Ecs Arch
traverseRoot as = do
  ecs <- get
  root <- VR.read (ecs ^. #archetypes) 0
  traverseArch root as

createArch :: VU.Vector TypeId -> Ecs (ArchId, Arch)
createArch tIds = do
  ecs <- get
  liftIO . putStrLn $ "createArch | creating arch of " <> show tIds
  newArchId <- MkArchId . tryFrom' <$> VR.length (ecs ^. #archetypes)
  newArch <- do
    entities' <- VR.new
    edges' <- do
      edgesLen <- liftIO $ from <$> readIORef (ecs ^. #types . #typeCtr)
      VUM.replicate edgesLen (MkEdge { add = Nothing, remove = Nothing }) >>= VR.toGrowable
    components' <- VG.forM (VG.convert tIds) \tId -> (ecs ^. #types . #typeCons) VG.! from tId

    pure $ MkArch
      { components = components'
      , entities = entities'
      , types = tIds
      , edges = edges'
      }
  VR.push (ecs ^. #archetypes) newArch
  -- Configure archetype generations
  untilM (\ct -> pure $ ct > VG.length tIds)
         (\ct -> VR.new >>= VR.push (ecs ^. #generations) >> pure (ct + 1))
         (VR.length $ ecs ^. #generations)
  -- Add to generations list
  VR.read (ecs ^. #generations) (VG.length tIds) >>= (`VR.push` newArchId)
  -- Update incoming & outgoing edges for previous-generation archetypes,
  -- this archetype, and next-generation archetypes.
  populateEdges (newArchId, newArch)
  pure (newArchId, newArch)

populateEdges :: (ArchId, Arch) -> Ecs ()
populateEdges (archId, arch) = do
  ecs <- get
  genCount <- VR.length (ecs ^. #generations)
  let thisGen           = VG.length $ arch ^. #types
      nextGen           = thisGen + 1
      prevGen           = thisGen - 1
      getGeneration gen =
        (ecs ^. #generations) `VR.read` gen
        >>= VR.freeze
        >>= VG.mapM (over (mapped . _1) (MkArchId . fromRight undefined . tryFrom)
                     . traverseOf _2 (((ecs ^. #archetypes) `VR.read`) . from))
        . VG.indexed
        . (VG.convert @_ @_ @V.Vector)

  -- | Write new edge information
  --   'thisDirection' is the edge direction to modify for 'arch'.
  --   'direction' is the edge direction to modify for either a previous- or next-gen
  --   archetype.
  let write direction thisDirection (archId', arch') = do
        let [diff] = (arch' ^. #types) `typeIdDiff` (arch ^. #types)
        VR.modify (arch' ^. #edges) (set direction (Just archId)) (from diff)
        VR.modify (arch ^. #edges) (set thisDirection (Just archId')) (from diff)
  unless (nextGen >= genCount) (getGeneration nextGen >>= VG.mapM_ (write #remove #add))
  unless (prevGen < 0) (getGeneration prevGen >>= VG.mapM_ (write #add #remove))

forQ :: Query q -> (QueryHandle q -> System ()) -> System ()
forQ q f = do
  VG.forM_ (q ^. #unQuery) \qh -> do
    entities <- VR.fromGrowable (qh ^. #trav . #currArch . #entities)
    VUM.iforM_ entities \idx (isValid, _) -> when isValid (f $ set #entity idx qh)

------------------------------------------------------------------------------------------
newtype Position = MkPosition Double
  deriving (Generic, Show)

derivingUnbox "Position" [t|Position -> Double|] [|\(MkPosition p) -> p|] [|MkPosition|]

instance Identify Position

newtype Velocity = MkVelocity Double
  deriving (Generic, Show)

derivingUnbox "Velocity" [t|Velocity -> Double|] [|\(MkVelocity v) -> v|] [|MkVelocity|]

instance Identify Velocity

myFunc = do
  createArch []
  ecs <- get
  let posSys (q :: Query (Nab Position |&| Nab Velocity)) = forQ q $ \qh -> do
        p <- nab @Position qh
        v <- nab @Velocity qh
        liftIO . putStrLn $ show p <> " " <> show v
      mySys :: Query (Nab Position |&| Nab Velocity) -> System ()
      mySys q = forQ q $ \qh -> do
        p <- nab @Position qh
        liftIO . print $ p
      myMaker c =
        VU.forM_ [0..1000] (\p -> void $ spawn c >>= tagged (MkPosition p) >>= tagged (MkVelocity p) >>= finish)
  (_, (MkSystemState commands)) <- queried myMaker >>= runSystem
  commitCommands (fromJust commands) >> queried posSys >>= runSystem
  pure ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
