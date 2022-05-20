{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.World where

import Control.Lens hiding (from)
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Data.HashMap.Internal qualified as HM
import Data.Vector qualified as V
import Data.Vector.Growable qualified as VR
import Data.Vector.Mutable qualified as VM
import Effectful
import Effectful.Prim (Prim)
import Effectful.State.Static.Local
import GHC.Base (Any)
import GHC.Generics (Generic)
import Recs.Archetype (Archetype, Archetypes, Edge (..), getStore')
import Recs.Core
import Recs.EntityInfo (EntityInfo)
import Recs.TypeInfo
import Recs.Utils
import Unsafe.Coerce (unsafeCoerce)
import Witch (from)

newtype Globals = MkGlobals {unGlobals :: GIOVector Any}
  deriving (Generic)

data World = MkWorld
  { archetypes :: Archetypes
  , globals :: Globals
  , entityInfo :: EntityInfo
  , typeInfo :: TypeInfo
  }
  deriving (Generic)

type Ecs es = (State World :> es, Prim :> es, IOE :> es)

{- | Retrieve this type's ID from the global type registry, adding any necessary
   information to the type registry.
-}

--   __Safety:__ This can modify all archetypes. Only safe during hard sync point.
identified :: forall c es. (Component c, Ecs es) => TypeInfo -> Eff es TypeId
identified tInfo = do
  case uncurry HM.lookup' (identify @c) tInfo.types of
    Just tId -> pure tId
    Nothing -> do
      let (h, f) = identify @c
      nextTId <- reserveTypeId tInfo
      let updateTInfo =
            let pushStorageDict = (`V.snoc` from (mkStorageDict @(Layout c)))
                pushNewType = HM.insert' h f nextTId
             in over #typeInfo (over #storageDicts pushStorageDict . over #types pushNewType)
      modify @World updateTInfo >> pure nextTId

getStore :: forall c es. (Component c, Ecs es) => Archetype -> Eff es (Maybe (Layout c))
getStore arch = get >>= identified @c . typeInfo <&> getStore' arch <&> unsafeCoerce @(Maybe Any) @(Maybe (Layout c))




















