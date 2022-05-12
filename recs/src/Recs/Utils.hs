module Recs.Utils
  (GIOVector
  ,GUIOVector
  ,GPIOVector
  ,GSIOVector
  ,Vector
  ,UVector
  ,IOVector
  ,UIOVector
  ,tryFrom'
  ,IOPVar) where

import           Data.Either                 (fromRight)
import           Data.Primitive.PVar         (PVar,RealWorld)
import qualified Data.Vector                 as V
import qualified Data.Vector.Growable        as VR
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Primitive       as VP
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Witch                       (TryFrom(..))

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
