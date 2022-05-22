{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Recs.Storage where

import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Deriving
import Recs.Utils
import Recs.Types
import Data.IORef (IORef, modifyIORef', readIORef, newIORef)


----------------------------------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------------------------------
instance Storage (GIOVector c) where
  type Elem (GIOVector c) = c

  storageInsert v _ e = VR.push v e

  storageRemove v _ idx =
    VR.pop v >>= \case
      Just e -> VR.write v idx e
      Nothing -> pure ()

  storageLookup v _ idx = VR.read v idx

  storageModify v _ idx e = VR.write v idx e

  storageInit = VR.new

instance VU.Unbox c => Storage (GUIOVector c) where
  type Elem (GUIOVector c) = c

  storageInsert v _ e = VR.push v e

  storageRemove v _ idx =
    VR.pop v >>= \case
      Just e -> VR.write v idx e
      Nothing -> pure ()

  storageLookup v _ idx = VR.read v idx

  storageModify v _ idx e = VR.write v idx e

  storageInit = VR.new

instance Storage (IORef (HM.HashMap EntityId c)) where
  type Elem (IORef (HM.HashMap EntityId c)) = c

  storageInsert h eId e = modifyIORef' h (HM.insert eId e)

  storageRemove h eId _ = modifyIORef' h (HM.delete eId)

  storageLookup h eId _ = readIORef h <&> fromJust . HM.lookup eId

  storageModify h eId _ e = modifyIORef' h (HM.insert eId e)

  storageInit = newIORef HM.empty

----------------------------------------------------------------------------------------------------
-- Components
----------------------------------------------------------------------------------------------------
newtype Boxed c = MkBoxed {unBoxed :: c}

instance Typeable c => Component (Boxed c) where
  type Layout (Boxed c) = GIOVector (Boxed c)

newtype Unboxed c = MkUnboxed {unUnboxed :: c}

derivingUnbox "Unboxed" [t|forall c. VU.Unbox c => Unboxed c -> c|] [|\(MkUnboxed c) -> c|] [|MkUnboxed|]

instance (Typeable c, VU.Unbox c) => Component (Unboxed c) where
  type Layout (Unboxed c) = GUIOVector (Unboxed c)

newtype Mapped c = MkMapped {unMapped :: c}

instance Typeable c => Component (Mapped c) where
  type Layout (Mapped c) = IORef (HM.HashMap EntityId c)













