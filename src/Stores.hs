{-# LANGUAGE DataKinds #-}

module Stores where

import Core
import qualified Data.Vector.Growable as VR
import Data.IORef (IORef)
import GHC.Generics (Generic)

-- | If a store's 'Layout' is 'Archetypal', then it will be stored in the
--   archetype graph.
--   If it is 'Flat', then it will be stored once in the global flat data field
--   of the ECS.
data StoreLayout
  = Archetypal
  | Flat
  deriving Generic

type family Layout s :: StoreLayout

newtype UMap c = MkUMap { unUMap :: VR.GrowableUnboxedIOVector c}
  deriving Generic

type instance Layout (UMap c) = Archetypal
type instance Elem (UMap c) = c

instance Component c => Get m (UMap c) where

newtype BMap c = MkBMap { unBMap :: VR.GrowableIOVector c }
  deriving Generic

type instance Layout (BMap c) = Archetypal
type instance Elem (BMap c) = c

newtype Global c = MkGlobal (IORef c)
  deriving Generic

type instance Layout (Global c) = Flat
type instance Elem (Global c) = c
