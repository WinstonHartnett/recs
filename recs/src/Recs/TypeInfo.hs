{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Recs.TypeInfo where

import Data.Either (fromRight)
import Data.HashMap.Internal (Hash)
import Data.HashMap.Strict qualified as HMS
import Data.Primitive.PVar
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)

import GHC.Base (Any)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Generics (Generic)

import Unsafe.Coerce

import Witch hiding (over)

import Data.Default (Default (def))
import Recs.Core
import Recs.Storage
import Recs.Utils

import Data.Vector.Growable qualified as VR

-- | Provide a unique hash for each type.
class Typeable t => Identify t where
  identify :: (Hash, Fingerprint)
  default identify :: (Hash, Fingerprint)
  --   TODO Generate these w/ TH
  identify = case typeRepFingerprint . typeRep $ Proxy @t of
    f@(Fingerprint h _) -> (fromRight (error "Word size mismatch") (tryFrom h), f)

instance {-# OVERLAPPABLE #-} Typeable t => Identify t

data SomeStorageDict = MkSomeStorageDict
  { storageInsert :: Any -> EntityId -> Any -> Any Any
  , storageRemove :: Any -> EntityId -> Int -> Any Any
  , storageLookup :: Any -> EntityId -> Int -> Any Any
  , storageModify :: Any -> EntityId -> Int -> Any -> Any Any
  , storageInit :: !(Any Any)
  }

data StorageDict m s = MkStorageDict
  { storageInsert :: s -> EntityId -> Elem s -> m s
  , storageRemove :: s -> EntityId -> Int -> m s
  , storageLookup :: s -> EntityId -> Int -> m (Elem s)
  , storageModify :: s -> EntityId -> Int -> Elem s -> m s
  , storageInit :: !(m s)
  }

instance From (StorageDict m s) SomeStorageDict where
  from = unsafeCoerce @(StorageDict m s) @SomeStorageDict

-- | Reify a 'Storage' instance.
mkStorageDict :: forall m a. Storage m a => StorageDict m a
mkStorageDict =
  MkStorageDict
    { storageInsert = storageInsert
    , storageRemove = storageRemove
    , storageLookup = storageLookup
    , storageModify = storageModify
    , storageInit = storageInit
    }

-- | Unsafely coerce an unqualified 'SomeStorageDict' to a 'StorageDict'.
unsafeCoerceStorageDict :: forall m a. Storage m a => SomeStorageDict -> StorageDict m a
unsafeCoerceStorageDict = unsafeCoerce

data TypeInfo = MkTypeInfo
  { nextTypeId :: {-# UNPACK #-} !(IOPVar TypeId)
  , types :: !(HMS.HashMap Fingerprint TypeId)
  , storageDicts :: {-# UNPACK #-} !(GIOVector SomeStorageDict)
  }
  deriving (Generic)

instance {-# OVERLAPPING #-} (MonadPrim RealWorld m) => Default (m TypeInfo) where
  def = do
    nextTypeId <- newPVar (MkTypeId 0)
    storageDicts <- VR.new
    pure $
      MkTypeInfo
        { nextTypeId = nextTypeId
        , types = HMS.empty
        , storageDicts = storageDicts
        }
