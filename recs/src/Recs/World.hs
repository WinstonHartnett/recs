{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Recs.World where

import qualified Data.HashMap.Internal as HM
import GHC.Base (Any)
import Recs.Archetype (Archetypes, Edge (..))
import Recs.Core
import Recs.EntityInfo (EntityInfo)
import Recs.TypeInfo
import Recs.Utils
import qualified Data.Vector.Growable as VR
import Witch (from)
import Effectful
import Effectful.State.Static.Local
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Control.Lens hiding (from)
import           Data.Generics.Labels ()
import qualified Data.Vector.Mutable as VM
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Vector as V

newtype Globals = MkGlobals {unGlobals :: GIOVector Any}
  deriving Generic

data World = MkWorld
  { archetypes :: Archetypes
  , globals :: Globals
  , entityInfo :: EntityInfo
  , typeInfo :: TypeInfo
  }
  deriving Generic

type Ecs es = (State World :> es, IOE :> es)

-- | Retrieve this type's ID from the global type registry, adding any necessary
--   information to the type registry.
--
--   __Safety:__ This can modify all archetypes. Only safe during hard sync point.
identified :: forall c es. (Component c, Ecs es) => Eff es TypeId
identified = do
  ecs <- get @World
  case uncurry HM.lookup' (identify @c) (ecs.typeInfo.types) of
    Just tId -> pure tId
    Nothing -> do
      let (h, f) = identify @c
      nextTId <- liftIO $ reserveTypeId $ ecs.typeInfo
      let pushNewTypeInfo =
            let pushNewDict = (`V.snoc` from (mkStorageDict @(Layout c)))
                pushId = HM.insert' h f nextTId
              in over #typeInfo (over #storageDicts pushNewDict . over #types pushId)
          pushNewEdges = do
            v <- VR.fromGrowable $ ecs ^. #archetypes . #unArchetypes
            VM.mapM_ (\a -> VR.push (a ^. #edges) (MkEdge Nothing Nothing)) v
          pushGlobals = VR.push (ecs ^. #globals . #unGlobals) (unsafeCoerce . error $ "Tried to access missing global")
      modify @World pushNewTypeInfo >> (liftIO $ pushNewEdges >> pushGlobals) >> pure nextTId





















