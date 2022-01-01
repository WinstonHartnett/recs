{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module MyLib where

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
import qualified Data.Set                     as S
import qualified Data.Sequence                as SQ
import           Data.Kind
import           Data.Maybe                   (fromJust,fromMaybe)
import           Data.Proxy                   (Proxy(..))
import qualified Data.Text                    as T
import           Data.Typeable                (Typeable,typeRep,typeRepFingerprint)
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Generic.Mutable  as VGM
import qualified Data.Vector.Growable         as VR
import qualified Data.Vector.Mutable          as VM
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

-- Query is reduced to a tree of operators and subjects

data Edge =
  MkEdge
  { add    :: !(Maybe ArchId)
  , remove :: !(Maybe ArchId)
  }
  deriving (Generic,Show)

derivingUnbox "Edge" [t|Edge -> (ArchId, ArchId)|] [|\(MkEdge a r)
  -> (fromMaybe (MkArchId (-1)) a, fromMaybe (MkArchId (-1)) r)|] [|\(a, r)
  -> let convertArchId i =
           if i == MkArchId (-1)
             then Nothing
             else Just i
     in MkEdge (convertArchId a) (convertArchId r)|]

data Arch =
  MkArch
  { components :: V.Vector Any
  , entities   :: VR.GrowableUnboxedIOVector (Bool, EntityId)
  , types      :: VU.Vector TypeId
  , edges      :: VR.GrowableUnboxedIOVector Edge
  }
  deriving Generic

data TypeInfo =
  MkTypeInfo
  { -- | Map a GHC Fingerprint to a unique type identifier.
    types    :: HM.HashMap Fingerprint TypeId
    -- | Next type identifier.
  , typeCtr  :: TypeId
    -- | Constructors for type-specific stores.
    --   These are needed to create new archetypes with unknown runtime types.
    --   By default, all types point to 'GrowableIOVector'.
  , typeCons :: V.Vector (Ecs Any)
  }
  deriving (Generic,Show)

-- TODO Not lawful
instance Show (Ecs a) where
  show e = "Ecs <value>"
-- instance Show (Ecs Any) where
--   show _ = "Ecs Any"

instance Default TypeInfo where
  def =
    MkTypeInfo
    { types   = HM.empty
    , typeCtr = MkTypeId 0
    , typeCons = V.empty
    }

data ArchRecord =
  MkArchRecord
  { archId :: ArchId
  , row    :: Int
  }
  deriving (Generic,Show)

data EntityInfo =
  MkEntityInfo
  { entityMap :: IM.IntMap ArchRecord
  , entityCtr :: EntityId
  }
  deriving (Generic,Show)

instance Default EntityInfo where
  def =
    MkEntityInfo
    { entityMap = IM.empty
    , entityCtr = MkEntityId 0
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
  def =
    MkWorld <$> VR.new <*> VR.new <*> pure def <*> pure def <*> VR.new <*> pure HM.empty

insertGlobal :: forall g. Component g => g -> Ecs ()
insertGlobal g = do
  ecs <- get
  globalId <- identified @g
  VR.write (ecs ^. #globals) (from globalId) (unsafeCoerce @g @Any g)

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

newtype Ecs a =
  MkEcs
  { unEcs :: StateT World IO a
  }
  deriving (Functor,Applicative,Monad,MonadState World,MonadIO,MonadThrow,MonadCatch
           ,MonadMask)

instance PrimMonad Ecs where
  type PrimState Ecs = RealWorld
  primitive = MkEcs . lift . IO

newtype Command = MkCommand (Ecs ())

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
newtype Commands = MkCommands
  { queue :: VR.GrowableIOVector Command
  }
  deriving Generic

data SystemState =
  MkSystemState
  { locals   :: V.Vector Any
  , commands :: Commands
  }
  deriving Generic


newtype SystemT m a = MkSystemT
  { unSystem :: ReaderT Commands m a
  }
  deriving (Generic, Functor, Applicative, Monad, MonadIO, MonadReader Commands, MonadTrans)

instance PrimMonad m => PrimMonad (SystemT m) where
  type PrimState (SystemT m) = PrimState m
  primitive = MkSystemT . primitive

type System a = SystemT Ecs a

-----------------------------------------------------------------------------------------
-- Archetype Graph
-----------------------------------------------------------------------------------------

untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM p f a = do
  cond <- a >>= p
  if cond
     then a
     else untilM p f (a >>= f)

class ArchStore a where
  type Elem a
  type Idx a
  getIdxS :: QueryTraversal -> Int -> Ecs (Idx a)
  initS :: Ecs a
  insertS :: a -> Elem a -> Ecs ()
  removeS :: a -> Idx a -> Ecs ()
  lookupS :: a -> Idx a -> Ecs (Elem a)
  modifyS :: a -> Idx a -> Elem a -> Ecs ()

instance ArchStore (VR.GrowableIOVector c) where
  type Elem (VR.GrowableIOVector c) = c
  type Idx (VR.GrowableIOVector c) = Int
  getIdxS _ i = pure i
  initS = VR.new
  insertS = VR.push
  removeS _ _ = pure ()
  lookupS = VR.read
  modifyS = VR.write

instance VU.Unbox c => ArchStore (VR.GrowableUnboxedIOVector c) where
  type Elem (VR.GrowableUnboxedIOVector c) = c
  type Idx (VR.GrowableUnboxedIOVector c) = Int
  getIdxS _ i = pure i
  initS = VR.new
  insertS = VR.push
  removeS _ _ = pure ()
  lookupS = VR.read
  modifyS = VR.write

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

registerType :: forall t. Identify t => Ecs ()
registerType =
  modify \ecs -> let (h, f) = identify @t
                     nextId = coerce $ ecs ^. #types . #typeCtr
                  in over #types (over #types (HM.insert' h f (MkTypeId nextId))
                                  . over #typeCtr (coerce (+ (1 :: Word16)))) ecs

data SystemAccess = MkSystemAccess
  { reads  :: VU.Vector TypeId
  , writes :: VU.Vector TypeId
  , tags   :: VU.Vector TypeId
  }
  deriving (Generic, Eq, Show)

instance Hashable SystemAccess

instance Default SystemAccess where
  def = MkSystemAccess
          { reads  = []
          , writes = []
          , tags   = []
          }

-- | 'Component' specifies which storage structure to use for each type.
--
--   Storage defaults to 'Unboxed' for performance reasons. You must derive
--   'Component via Boxed' if you want Boxed storage.
class (ArchStore (Storage c), Identify c) => Component (c :: Type) where
  type Storage c

type Boxed c = VR.GrowableIOVector c

type Unboxed c = VR.GrowableUnboxedIOVector c

instance {-# OVERLAPPABLE #-} (VU.Unbox c, Identify c) => Component c where
  type Storage c = Unboxed c

-- | Retrieve this type's ID from the global type registry, adding any necessary
--   information to the type registry.
identified :: forall c. Component c => Ecs TypeId
identified = do
  ecs <- get
  case uncurry HM.lookup' (identify @c) (ecs ^. (#types . #types)) of
    Just tId -> pure tId
    Nothing -> do
      let (h, f) = identify @c
          nextTId = ecs ^. #types . #typeCtr
          updateTInfo =
            let addTCon tCons = VG.snoc tCons (unsafeCoerce <$> initS @(Storage c))
             in over #types (over #typeCons addTCon
                              . over #types (HM.insert' h f nextTId)
                              . over #typeCtr (\(MkTypeId t) -> MkTypeId $ t + 1))
          pushNewEdges = VR.fromGrowable (ecs ^. #archetypes) >>= VM.mapM_ (\a -> do
                          VR.push (a ^. #edges) (MkEdge Nothing Nothing))
          pushGlobals = VR.push (ecs ^. #globals) (unsafeCoerce . error $ "Tried to access missing global '" <> show (typeRep $ Proxy @c) <> "'.")
       in modify updateTInfo >> pushNewEdges >> pushGlobals >> pure nextTId

instance (VU.Unbox a, Hashable a) => Hashable (VU.Vector a) where
  -- TODO Slow
  hashWithSalt i = hashWithSalt i . VG.toList

--------------------------------------------------------------------------------
-- | Retrieves a system argument. For queries, this means either loading results
--   from cache or executing a query and caching it.
class Queryable q where
  -- FIXME Redo this whole setup to properly use System
  query :: System q

tryFrom' :: TryFrom a b => a -> b
tryFrom' = fromRight undefined . tryFrom

instance DesugarQuery q => Queryable (Query q) where
  query = do
    res <- lift $ desugarQuery @q >>= runQuery
    pure . MkQuery . V.map (MkQueryHandle 0) . unSomeQuery $ res

-- TODO loosen superclass bound
instance Component c => Queryable (Global c) where
  query = do
    ecs <- get
    globalId <- lift $ identified @c
    unsafeCoerce @_ @(Global c) <$> VR.read (ecs ^. #globals) (from globalId)

instance Queryable (Local c) where
  query = undefined

instance Queryable Commands where
  query = ask

-----------------------------------------------------------------------------------------
-- | Get the return type of a function.
type family Returns s where
  Returns (a -> b) = Returns b
  Returns (System a) = (a, Commands)

-- | Executes system, applying arguments as needed.
class Queried s where
  queried :: s -> System (Returns s)

instance (Queryable p, Queried s) => Queried (p -> s) where
  queried s = (s <$> query @p) >>= queried

-- Systems must eventually return a value.
instance Queried (System a) where
  queried sys = do
    (,) <$> sys <*> ask
    -- (,) <$> (runReaderT (unSystem sys) =<< ask) <*> ask
    -- newCommands <- MkCommands <$> VR.new
    -- (,) <$> flip runReaderT newCommands (unSystem sys) <*> pure newCommands
    -- TODO Move this to the global command commit queue
    -- UNSAFE FOR PARALLEL EXECUTION!!
    -- VR.unsafeFreeze (queue commands) >>= VG.mapM_ (\(MkCommand c) -> c)
    -- pure res

type family Layout c


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
             then pure $ Just idx
             else loop (idx + 1)

make :: forall c. Component c => Commands -> c -> System ()
make commands c = do
  (MkCommands commands') <- ask
  VR.push commands' $ MkCommand $ do
    ident <- identified @c
    targetArch <- traverseRoot (Add [ident])
    let targetStore = unsafeCoerce @_ @(Storage c) $ (targetArch ^. #components) VG.! from ident
    -- -- TODO For now we just traverse the entityIds until we find a hidey hole
    nextIdx <- findNextAvailEntity =<< VR.fromGrowable (targetArch ^. #entities)
    case nextIdx of
      -- TODO Remember ->>>> multiple components!
      Just idx -> modifyS targetStore idx c
      Nothing -> insertS targetStore c

destroy = undefined

-----------------------------------------------------------------------------------------
-- Scheduler
-----------------------------------------------------------------------------------------

newtype Label = MkLabel { unLabel :: T.Text }
  deriving Generic

data SystemSchedule a = MkSystemSchedule
  { system :: System a
  , labels :: V.Vector Label
  , before :: V.Vector Label
  , after  :: V.Vector Label
  }
  deriving Generic

chain :: SystemSchedule a -> SystemSchedule (a -> b) -> Schedule (SystemSchedule b)
chain a b = do
  undefined

newtype Schedule a = MkSchedule
  { unSchedule :: StateT Int Ecs a
  }
  deriving (Generic)

-----------------------------------------------------------------------------------------
-- newtype Player = MkPlayer T.Text
-- newtype Score = MkScore Word64
-- data GameState = MkGameState
--   { currRound :: Word
--   , totPlayers :: Word
--   , winningPlayer :: Maybe T.Text
--   }
--   deriving Generic
-- data GameRules = MkGameRules
--   { winningScore :: Word
--   , maxRounds :: Word
--   , maxPlayers :: Word
--   }
--   deriving Generic

-- newRoundSystem :: Global (Nab GameRules) -> Global (Stow GameState) -> System ()
-- newRoundSystem gr' gs' = do
--   (gs, gr) <- liftA2 (,) (nab gs') (nab gr')
--   stow gs' $ over #currRound (+1) gs
--   liftIO . putStrLn $
--     "Begin round " ++ (gs ^. #currRound) ++ " of " ++ (gr ^. #maxRounds)

-- scoreSystem :: Query (Nab Player |&| Stow Score) -> System ()
-- scoreSystem q = forQ q $ \q' -> do
--   (p, s) <- nab q' @(Player, Score)
--   scoredAPoint <- rand
--   if scoredAPoint
--     then do
--       stow q' (over #unwrapped (+1) s)
--       putStrLn $
--         show (p ^. #name) ++
--         " scored a point! Their score is: " ++
--         show (s ^. #unwrapped)
--     else do
--       putStrLn $
--         show (p ^. #name) ++
--         " did not score a point! Their score is: " ++
--         show (s ^. #unwrapped)
-- scoreCheckSystem ::
--   Global (Nab GameRules |&| Stow GameState) ->
--   Query (Nab (Player, Score)) ->
--   System ()
-- scoreCheckSystem gr' gs' q' =
--   forQ q' $ \q -> do
--     ((p, s), gr, gs) <- (,,) <$> nab q @(Player, Score) <*> nab gr' <*> nab gs'
--     when (s ^. #unwrapped == gr ^. #winningScore)
--          (stow gs' $ set #winningPlayer (Just $ p ^. #name) gs)

-- fn debug_player_hp(
--     // access the health, only for friendly players, optionally with name
--     query: Query<(&Health, Option<&PlayerName>), (With<Player>, Without<Enemy>)>,
-- ) {
--     // get all matching entities
--     for (health, name) in query.iter() {
--         if let Some(name) = name {
--             eprintln!("Player {} has {} HP.", name.0, health.hp);
--         } else {
--             eprintln!("Unknown player has {} HP.", health.hp);
--         }
--     }
-- }

-- debugPlayerHP ::
--    Query (Nab Health
--       |&| Nab (Maybe PlayerName)
--       |&| With Player
--       |&| Without Enemy) ->
--    System ()
-- debugPlayerHP q = forQ q $ \q' -> do
--   (h, n) <- nab q'
--   case n of
--     Just name -> putStrLn $ "Player " <> show (fst name) <> " has " <> show (h ^. #hp) <> " HP."
--     Nothing -> putStrLn $ "Unknown player has " <> show (h ^. #hp) " HP."


-- data Flying = MkFlying

-- data Position = MkPosition Double
--   deriving Show

-- derivingUnbox "Position" [t|Position -> Double|]
--   [|\(MkPosition d) -> d|]
--   [|MkPosition|]

-- data Velocity = MkVelocity Double

-- derivingUnbox "Velocity" [t|Velocity -> Double|]
--   [|\(MkVelocity d) -> d|]
--   [|MkVelocity|]

-- instance Identify Position

-- instance Identify Velocity

-- myFunc = do
--   createArch []
--   createArch . VG.convert =<< V.sequence [identified @Position, identified @Velocity]
--   createArch . VG.convert =<< V.sequence [identified @Position]
--   q  <- query @(Query (Nab Position |&| Nab Velocity))
--   q' <- query @(Query (Nab Position))
--   -- insertGlobal (MkPosition 1.0)
--   -- g <- query @(Global Position)
--   -- liftIO . print $ unGlobal g
--   -- let mySys = forQ q $ \qh -> do
--   --       liftIO $ print "Hello"
--   --       p <- nab @Position qh
--   --       pure ()
--   let mySys :: Global Position -> Query (Nab Position |&| Nab Velocity) -> System ()
--       mySys g q = forQ q $ \qh -> do
--         liftIO . print $ unGlobal g
--         p <- nab @Position qh
--         com <- ask
--         make com (MkPosition 1.0)
--         liftIO . print $ p
--       myMakerSys = do
--         com <- ask
--         make com (MkPosition 1.0)
--   (_, commands) <- queried myMakerSys
--   -- liftIO . print =<< VR.length (queue commands)
--   -- (_, commands) <- queried mySys
--   pure ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
