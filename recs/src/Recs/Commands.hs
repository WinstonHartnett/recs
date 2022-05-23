{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Recs.Commands where

import Data.Coerce (coerce)
import Data.Hashable (Hashable (hash))
import Data.IntSet qualified as IS
import Data.Sequence qualified as SQ
import Data.Vector.Unboxed qualified as VU
import Effectful
import Effectful.State.Static.Local
import Recs.Archetype (getArchByArchId, nextIdx, traverseArch, unsafeGetStore')
import Recs.EntityInfo
import Recs.Types
import Recs.Utils
import Data.Default (Default(..))

{- | Given a set of Commands, compute the archetype moves necessary.
   __Beware:__ This constructs the Archetypes described in Commands.
-}
constructArchetypeMove :: Ecs es => EntityCommands -> Eff es (Maybe ArchetypeMove)
constructArchetypeMove ec = do
  startingMeta <- getEntityMeta ec.entityId
  startingArch <- getArchByArchId startingMeta.location.archId
  let consolidateIds com (adds', removes', _) = case com of
        MkTag f _ -> (IS.insert (hash f) adds', removes', False)
        MkUntag f _ -> (adds', IS.delete (hash f) removes', False)
        MkDespawn -> (IS.empty, IS.empty, True)
  let (adds, removes, isDespawn) = foldr consolidateIds ([], [], False) ec.queue
  -- First find the Archetype formed by the net removal of target components.
  -- Decreases the Archetype generation.
  removeArch <-
    traverseArch
      startingArch
      ( Remove
          . coerce
          . VU.fromList
          . IS.toList
          $ intSetDiff removes adds
      )
  -- Then find the archetype formed by the net additions of target components.
  addArch <-
    traverseArch
      removeArch
      ( Add
          . coerce
          . VU.fromList
          . IS.toList
          $ intSetDiff adds removes
      )
  nextAddIdx <- nextIdx addArch
  if isDespawn
    then
      pure
        . Just
        $ MovedArchetype
          { src = startingMeta.location
          , dest = MkEntityRecord addArch.archId nextAddIdx
          }
    else pure Nothing

-- onEntity :: forall es. System es => EntityId -> Eff (State (EntityCommands es) : es) () -> Eff es ()
-- onEntity eId cb = do
--   (_, eCommands) <- runState (MkEntityCommands eId SQ.empty) cb
--   modify @(SystemState es) (over (#commands . #unCommands) (SQ.|> eCommands))

-- spawn :: System es => Eff (State (EntityCommands es) : es) () -> Eff es EntityId
-- spawn cb = do
--   newEId <- reserveEntityId
--   onEntity newEId cb
--   pure newEId

-- runCommandBuilder :: forall cb cx es a. (CommandBuilderE (cb : cx) :> (cb : cx)) => EntityId -> Eff (cb : cx) a -> Eff es (a, EntityCommands cx)
-- runCommandBuilder eId cb = do
--   let a = runState (MkEntityCommands eId SQ.empty)
--   undefined

insertionCommit :: forall c es. (Ecs es, Component c) => c -> EntityId -> ArchetypeMove -> Eff es ()
insertionCommit c eId = \case
  MovedArchetype{src, dest} -> do
    srcStore <- locateStore src.archId
    destStore <- locateStore dest.archId
    liftIO do
      storageInsert destStore eId c
      storageRemove srcStore eId dest.idx
  SameArchetype (MkEntityRecord{archId, idx}) -> do
    store <- locateStore archId
    liftIO $ storageModify store eId idx c
 where
  locateStore aId = getArchByArchId aId >>= unsafeGetStore' @c

-- tagged :: forall c cb. (CommandBuilderE cb :> cb, Ecs cb, Component c) => c -> Eff cb ()
-- tagged c = do
--   modify @(EntityCommands cb) \ec ->
--     let tagCommand =
--           MkTag
--             { fingerprint = snd $ identify @c
--             , commit = insertionCommit c ec.entityId
--             }
--      in over #queue (SQ.|> tagCommand) ec
