{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Recs.Types where

import Control.Lens hiding (from)
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Data.HashMap.Internal qualified as HM
import Data.Vector qualified as V
import Data.Vector.Growable qualified as VR
import Data.Vector.Mutable qualified as VM
import Data.Maybe (fromMaybe)
import Effectful
import Effectful.Prim (Prim)
import Effectful.State.Static.Local
import GHC.Base (Any)
import GHC.Generics (Generic)
-- import Recs.Archetype (Archetype, Archetypes, Edge (..), getStore')
import Recs.Core
-- import Recs.EntityInfo (EntityInfo)
-- import Recs.TypeInfo
import Recs.Utils
import Unsafe.Coerce (unsafeCoerce)
import Witch (From(..))
import Data.Vector.Unboxed.Deriving
import Data.Word (Word32)
import qualified Data.HashMap.Strict as HMS
import GHC.Fingerprint (Fingerprint)
import qualified Data.Sequence as SQ

newtype Globals = MkGlobals {unGlobals :: GIOVector Any}
  deriving (Generic)

----------------------------------------------------------------------------------------------------
-- Data
----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------
-- Entity Information
----------------------------------------------------------------------------------------------------
data EntityRecord = MkEntityRecord
  { archId :: !ArchId
  , idx :: !Int
  }
  deriving (Generic, Show)

derivingUnbox
  "EntityRecord"
  [t|EntityRecord -> (ArchId, Int)|]
  [|
    \(MkEntityRecord a i) ->
      (a, i)
    |]
  [|uncurry MkEntityRecord|]

data EntityMeta = MkEntityMeta
  { generation :: !Word32
  , location :: !EntityRecord
  }
  deriving (Generic)

derivingUnbox
  "EntityMeta"
  [t|EntityMeta -> (Word32, EntityRecord)|]
  [|\(MkEntityMeta g l) -> (g, l)|]
  [|uncurry MkEntityMeta|]

-- | Records information about Entities stored in the ECS.
data EntityInfo = MkEntityInfo
  { records :: {-# UNPACK #-} !(GUIOVector EntityMeta)
  -- ^ 'records' is indexed by an EntityId
  , pending :: {-# UNPACK #-} !(GUIOVector EntityId)
  , freeCursor :: {-# UNPACK #-} !(IOPVar Int)
  , len :: {-# UNPACK #-} !(IOPVar Int)
  }
  deriving (Generic)

----------------------------------------------------------------------------------------------------
-- Type Information
----------------------------------------------------------------------------------------------------

data SomeStorageDict = MkSomeStorageDict
  { _storageInsert :: Any -> EntityId -> Any -> Any Any
  , _storageRemove :: Any -> EntityId -> Int -> Any Any
  , _storageLookup :: Any -> EntityId -> Int -> Any Any
  , _storageModify :: Any -> EntityId -> Int -> Any -> Any Any
  , _storageInit :: !(Any Any)
  }

data StorageDict s = MkStorageDict
  { _storageInsert :: s -> EntityId -> Elem s -> IO s
  , _storageRemove :: s -> EntityId -> Int -> IO s
  , _storageLookup :: s -> EntityId -> Int -> IO (Elem s)
  , _storageModify :: s -> EntityId -> Int -> Elem s -> IO s
  , _storageInit :: !(IO s)
  }

instance From (StorageDict s) SomeStorageDict where
  from = unsafeCoerce @(StorageDict s) @SomeStorageDict

-- | Reify a 'Storage' instance.
mkStorageDict :: forall a. Storage a => StorageDict a
mkStorageDict =
  MkStorageDict
    { _storageInsert = storageInsert
    , _storageRemove = storageRemove
    , _storageLookup = storageLookup
    , _storageModify = storageModify
    , _storageInit = storageInit
    }

data TypeInfo = MkTypeInfo
  { nextTypeId :: {-# UNPACK #-} !(IOPVar Int)
  -- ^ Can only be accessed atomically.
  , types :: !(HMS.HashMap Fingerprint TypeId)
  , storageDicts :: !(V.Vector SomeStorageDict)
  }
  deriving (Generic)

----------------------------------------------------------------------------------------------------
-- Archetypes
----------------------------------------------------------------------------------------------------

-- | An edge in the Archetype graph.
data Edge = MkEdge
  { add :: !(Maybe ArchId)
  , remove :: !(Maybe ArchId)
  }
  deriving (Generic, Show)

derivingUnbox
  "Edge"
  [t|Edge -> (ArchId, ArchId)|]
  [|
    \e ->
      (fromMaybe invalidArchId e.add, fromMaybe invalidArchId e.remove)
    |]
  [|
    \(add, remove) ->
      let convertArchId i
            | invalidArchId == i = Nothing
            | otherwise = Just i
       in MkEdge (convertArchId add) (convertArchId remove)
    |]

data Archetype = MkArch
  { components :: {-# UNPACK #-} !(Vector Any)
  , entities :: {-# UNPACK #-} !(GUIOVector EntityId)
  , types :: {-# UNPACK #-} !(UVector TypeId)
  , edges :: {-# UNPACK #-} !(GUIOVector Edge)
  }
  deriving (Generic)

newtype Archetypes = MkArchetypes { unArchetypes :: GIOVector Archetype }
  deriving (Generic)

----------------------------------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------------------------------

data ArchetypeMove
  = MovedArchetype
      { src :: {-# UNPACK #-} !EntityRecord
      , dest :: {-# UNPACK #-} !EntityRecord
      }
  | SameArchetype {-# UNPACK #-} !EntityRecord

data Command m
  = MkTag
      { fingerprint :: !Fingerprint
      , commit :: !(World -> m ())
      }
  | MkUntag
      { fingerprint :: !Fingerprint
      , commit :: !(World -> m ())
      }
  | Despawn

data EntityCommands m = MkEntityCommands
  { entityId :: !EntityId
  , queue :: SQ.Seq (Command m)
  }

newtype Commands m = MkCommands {unCommands :: SQ.Seq (EntityCommands m)}

type CommandBuilder m = State (EntityCommands m)

----------------------------------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------------------------------

-- | System-local state.
data SystemState m = MkSystemState
  { commands :: !(Commands m)
  , world :: !World
  }

type System m es = (State (SystemState m) :> es, Ecs es)

data World = MkWorld
  { archetypes :: Archetypes
  , globals :: Globals
  , entityInfo :: EntityInfo
  , typeInfo :: TypeInfo
  }
  deriving (Generic)

type Ecs es = (State World :> es, Prim :> es, IOE :> es)




































































































