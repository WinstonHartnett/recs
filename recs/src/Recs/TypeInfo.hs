{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.TypeInfo where

import Control.Monad.State.Strict (MonadIO (liftIO))
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Either (fromRight)
import Data.HashMap.Internal qualified as HMS
import Data.Primitive.PVar
import Data.Vector.Growable qualified as VR
import GHC.Base (Any)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Generics (Generic)
import Recs.Core
import Recs.Utils (GIOVector, GUIOVector, IOPVar, tryFrom')
import Unsafe.Coerce (unsafeCoerce)
import Witch hiding (over)
import Effectful
import qualified Data.Vector as V

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

-- | Unsafely coerce an unqualified 'SomeStorageDict' to a 'StorageDict'.
unsafeCoerceStorageDict :: forall a. Storage a => SomeStorageDict -> StorageDict a
unsafeCoerceStorageDict = unsafeCoerce

data TypeInfo = MkTypeInfo
  { nextTypeId :: {-# UNPACK #-} !(IOPVar Int)
  -- ^ Can only be accessed atomically.
  , types :: !(HMS.HashMap Fingerprint TypeId)
  , storageDicts :: !(V.Vector SomeStorageDict)
  }
  deriving (Generic)

instance {-# OVERLAPPING #-} (MonadIO m, MonadPrim RealWorld m) => Default (m TypeInfo) where
  def = do
    nextTypeId <- newPVar 0
    -- storageDicts <- VR.new
    pure $
      MkTypeInfo
        { nextTypeId = nextTypeId
        , types = HMS.empty
        , storageDicts = V.empty
        }

reserveTypeId :: MonadPrim RealWorld m => TypeInfo -> m TypeId
reserveTypeId tInfo = atomicModifyIntPVar tInfo.nextTypeId \tId -> (tId + 1, tryFrom' tId)


















































