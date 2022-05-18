{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Recs.Storage where

import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Data.Vector.Growable qualified as VR
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Deriving
import Recs.Core
import Recs.Utils

{- Component wrappers.
-}
newtype Boxed c = MkBoxed {unBoxed :: c}

instance Typeable c => Component (Boxed c) where
  type Layout (Boxed c) = GIOVector (Boxed c)

newtype Unboxed c = MkUnboxed {unUnboxed :: c}

instance (Typeable c, VU.Unbox c) => Component (Unboxed c) where
  type Layout (Unboxed c) = GUIOVector (Unboxed c)

derivingUnbox "Unboxed" [t|forall c. VU.Unbox c => Unboxed c -> c|] [|\(MkUnboxed c) -> c|] [|MkUnboxed|]

newtype Mapped c = MkMapped {unMapped :: c}

instance Typeable c => Component (Mapped c) where
  type Layout (Mapped c) = HM.HashMap EntityId c

instance Storage (GIOVector c) where
  type Elem (GIOVector c) = c

  storageInsert v _ e = VR.push v e >> pure v

  storageRemove v _ idx =
    -- Moves the last element to fill the gap left by the 'idx' removal
    VR.pop v >>= maybe (pure v) \e -> VR.write v idx e >> pure v

  storageLookup v _ idx = VR.read v idx

  storageModify v _ idx e = VR.write v idx e >> pure v

  storageInit = VR.new

instance VU.Unbox c => Storage (GUIOVector c) where
  type Elem (GUIOVector c) = c

  storageInsert v _ e = VR.push v e >> pure v

  storageRemove v _ idx = do
    VR.pop v >>= maybe (pure v) \e -> VR.write v idx e >> pure v

  storageLookup v _ idx = VR.read v idx

  storageModify v _ idx e = VR.write v idx e >> pure v

  storageInit = VR.new

instance Storage (HM.HashMap EntityId c) where
  type Elem (HM.HashMap EntityId c) = c

  storageInsert h eId e = pure $ HM.insert eId e h

  storageRemove h eId _ = pure $ HM.delete eId h

  storageLookup h eId _ = pure . fromJust $ HM.lookup eId h

  storageModify h eId _ e = pure $ HM.insert eId e h

  storageInit = pure $ HM.empty
