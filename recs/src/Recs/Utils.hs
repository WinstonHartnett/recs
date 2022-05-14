module Recs.Utils (
  GIOVector,
  GUIOVector,
  GPIOVector,
  GSIOVector,
  Vector,
  UVector,
  IOVector,
  UIOVector,
  tryFrom',
  IOPVar,
) where

import Data.Either (fromRight)
import Data.Primitive.PVar (PVar, RealWorld)
import Data.Vector qualified as V
import Data.Vector.Growable qualified as VR
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

import Witch (TryFrom (..))

-- Vector Synonyms
type GIOVector = VR.GrowableIOVector

type GUIOVector = VR.GrowableUnboxedIOVector

type GPIOVector = VR.GrowablePrimitiveIOVector

type GSIOVector = VR.GrowableStorableIOVector

type Vector = V.Vector

type UVector = VU.Vector

type IOVector = VM.IOVector

type UIOVector = VUM.IOVector

-- Conversion stuff
tryFrom' :: TryFrom a b => a -> b
tryFrom' = fromRight undefined . tryFrom

type IOPVar a = PVar a RealWorld
