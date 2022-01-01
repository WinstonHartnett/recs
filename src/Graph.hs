module Graph where

data ArchSearch
  = Add (VU.Vector TypeId)
  | Remove (VU.Vector TypeId)

traverseArch :: Arch -> ArchSearch -> Ecs Arch
traverseArch root as = do
  ecs <- get
  let (tId, edgeLens) = case as of
        Add t    -> (t, #add)
        Remove t -> (t, #remove)
      loop tId' root' = case VG.uncons tId' of
        Just (nextTId, stack) -> do
          targetEdge <- view edgeLens <$> (root' ^. #edges) `VR.read` from nextTId
          nextRoot <- case targetEdge of
            Just r  -> (ecs ^. #archetypes) `VR.read` from r
            Nothing -> undefined -- TODO create archetype
          loop stack nextRoot
        Nothing -> pure root'
  loop tId root

traverseRoot :: ArchSearch -> Ecs Arch
traverseRoot as = do
  ecs <- get
  root <- VR.read (ecs ^. #archetypes) 0
  traverseArch root as

createArch :: VU.Vector TypeId -> Ecs Arch
createArch tIds = do
  ecs <- get
  newArchId <- MkArchId . tryFrom' <$> VR.length (ecs ^. #archetypes)
  newArch <- do
    entities' <- VR.new
    edges' <-
      let edgesLen = from $ ecs ^. #types . #typeCtr
       in VUM.replicate edgesLen (MkEdge { add = Nothing, remove = Nothing }) >>= VR.toGrowable
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
  pure newArch

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
