{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Recs.Commands where

import Control.Monad (foldM, forM_, void)
import Data.Coerce (coerce)
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as SQ
import Data.Vector.Unboxed qualified as VU
import Effectful
import Effectful.State.Static.Local (modify, runState)
import Recs.Archetype (getArchByArchId, nextIdx, traverseArch, unsafeGetStore')
import Recs.EntityInfo
import Recs.TypeInfo (identifiedByFingerprint)
import Recs.Types
import Recs.Utils

{- | Given a set of Commands, compute the archetype moves necessary.
   __Beware:__ This constructs the Archetypes described in Commands.
-}
constructArchetypeMove :: Ecs es => EntityCommands -> Eff es (Maybe (ArchetypeMove, SQ.Seq Command))
constructArchetypeMove ec = do
  liftIO $ putStrLn "constructArchetypeMove | start"
  startingMeta <- getEntityMeta ec.entityId
  startingArch <- getArchByArchId startingMeta.location.archId
  liftIO $ putStrLn "constructArchetypeMove | acquired meta"

  let coms = case ec.payload of
        MkSpawned s -> Just s
        MkOperated s -> Just s
        MkDespawned -> Nothing
  case coms of
    Just coms' -> do
      (adds, removes) <- foldM consolidateIds ([], []) coms'
      removeArch <- traverseArch startingArch (Remove $ intSetToTypeIdVec removes adds)
      addArch <- traverseArch removeArch (Add $ intSetToTypeIdVec adds removes)
      nextAddIdx <- nextIdx addArch
      pure $
        Just $
          ( MovedArchetype
            { src = startingMeta.location
            , dest = MkEntityRecord addArch.archId nextAddIdx
            }
          , coms'
          )
    Nothing -> pure Nothing
 where
  consolidateIds (a, r) com = case com of
    MkTag f d _ -> do
      tId <- identifiedByFingerprint f d
      pure (IS.insert (from tId) a, r)
    MkUntag f d _ -> do
      tId <- identifiedByFingerprint f d
      pure (a, IS.insert (from tId) r)
  intSetToTypeIdVec a b = coerce . from @[Int] @(VU.Vector Int) . from $ intSetDiff a b

-- | Run a Command builder in a system without adding its commands to the queue.
runCommandBuilder :: EntityId -> CommandVariant -> Eff (CommandBuilderE : cb) a -> Eff cb (a, EntityCommands)
runCommandBuilder eId cv = runState (MkEntityCommands eId cv)

-- | Run a 'CommandBuilder' action and add it to the current system's queue.
runCommandBuilder' :: System cb => EntityId -> CommandVariant -> Eff (CommandBuilderE : cb) a -> Eff cb (a, EntityCommands)
runCommandBuilder' eId cv cb = do
  (a, eCommands) <- runCommandBuilder eId cv cb
  modify @SystemState (over (#commands . #unCommands) (SQ.|> eCommands))
  pure (a, eCommands)

-- | Apply the given commands to the specified Entity.
onEntity :: System es => EntityId -> Eff (CommandBuilderE : es) () -> Eff es ()
onEntity eId cb = void $ runCommandBuilder' eId (MkOperated SQ.empty) cb

-- | Spawn a new Entity and apply the given commands.
spawn :: System es => Eff (CommandBuilderE : es) () -> Eff es EntityId
spawn cb = do
  newEId <- reserveEntityId
  void $ runCommandBuilder' newEId (MkSpawned SQ.empty) cb
  pure newEId

insertionCommit :: forall c es. (Ecs es, Component c) => c -> EntityId -> ArchetypeMove -> Eff es ()
insertionCommit c eId am = do
  case am of
    MovedArchetype{src, dest} -> do
      liftIO $ putStrLn "insertionCommit | MovedArchetype"
      srcStore <- locateStore src.archId
      destStore <- locateStore dest.archId
      liftIO do
        storageInsert destStore eId c
        storageRemove srcStore eId dest.idx
    SameArchetype (MkEntityRecord{archId, idx}) -> do
      liftIO $ putStrLn "insertionCommit | SameArchetype"
      store <- locateStore archId
      liftIO $ storageModify store eId idx c
 where
  locateStore aId = getArchByArchId aId >>= unsafeGetStore' @c

tagged :: forall c cb. (CommandBuilderE :> cb, Component c) => c -> Eff cb ()
tagged c =
  modify @EntityCommands \ec ->
    let tagCommand =
          MkTag
            { fingerprint = snd $ identify @c
            , storageDict = toSomeStorageDict $ mkStorageDict @(Layout c)
            , commit = insertionCommit c ec.entityId
            }
     in over
          #payload
          ( \case
              MkSpawned s -> MkSpawned (s SQ.|> tagCommand)
              MkOperated s -> MkOperated (s SQ.|> tagCommand)
              MkDespawned -> MkDespawned
          )
          ec
commitCommands :: Ecs es => Commands -> Eff es ()
commitCommands coms = do
  liftIO $ putStrLn "commitCommands | start"
  forM_ coms.unCommands \ec -> do
    move <- constructArchetypeMove ec
    case move of
      Just (move', queue) ->
        forM_ queue \case
          MkTag _ _ c -> inject $ c move'
          MkUntag _ _ c -> inject $ c move'
      Nothing -> do
        -- Despawned
        undefined
