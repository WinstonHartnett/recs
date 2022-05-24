module Recs.Utils (module Recs.Utils, module Control.Lens, module Witch) where

import Control.Lens hiding (from)
import Data.Either (fromRight)
import Data.Primitive.PVar (PVar, RealWorld)
import Data.Vector qualified as V
import Data.Vector.Growable qualified as VR
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Generics.Labels ()
import qualified Data.IntSet as IS

import Witch (From (..), TryFrom (..))

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

intSetDiff :: IS.IntSet -> IS.IntSet -> IS.IntSet
intSetDiff a b = IS.union a b `IS.difference` IS.intersection a b

instance From [a] (V.Vector a) where
  from = V.fromList

instance VU.Unbox a => From [a] (VU.Vector a) where
  from = VU.fromList
