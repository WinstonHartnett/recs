module Query where

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

basicHas :: QuerySubject -> QueryRelevant
basicHas qs = do
  (a, _) <- ask
  let targetStore = \tId -> ((a ^. #components) VG.!) <$> VU.findIndex (== tId) (a ^. #types)
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
        let queryEntries = IM.toList queryRes
            matched' = VG.create $ do
              v <- VGM.new $ from (ecs ^. #types . #typeCtr)
              forM_ queryEntries (\(i, a) -> VGM.write v i a)
              pure v
            matchedOk' = VG.create $ do
              v <- VGM.new $ from (ecs ^. #types . #typeCtr)
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

-----------------------------------------------------------------------------------------
-- Access Control
-----------------------------------------------------------------------------------------
-- type family IsSubstantive q :: Bool where
--   IsSubstantive (With _) = False
--   IsSubstantive (Not _) = False
--   IsSubstantive (Tag _) = False
--   IsSubstantive (a |&| b) = Or (IsSubstantive a) (IsSubstantive b)
--   IsSubstantive (a ||| b) = Or (IsSubstantive a) (IsSubstantive b)
--   IsSubstantive _ = True

-- type WhenSubstantive a r = If (IsSubstantive a) r '[]

-- | Optional query wrapper.
data Opt c

type family QueryItems' a b where
  QueryItems' a (b ||| bs) = '[Opt a] ++ QueryItems (b ||| bs)
  QueryItems' a b = '[Opt a] ++ '[Opt (QueryItems b)]

-- | Used to check System-local access permissions, i.e. when using 'nab' or similar
--   'QueryHandle' functions.
type family QueryItems q :: [Type] where
  QueryItems (Nab a) = '[Nab a]
  QueryItems (Stow a) = '[Stow a]
  QueryItems (a |&| b) = QueryItems a ++ QueryItems b
  QueryItems (a ||| b) = QueryItems' (QueryItems a) b

getStore :: forall c q. Component c => QueryHandle q -> Ecs (Storage c)
getStore qh = do
  ident <- identified @c
  pure $ unsafeCoerce @_ @(Storage c) $ (qh ^. #trav . #currArch . #components) VG.! from ident
