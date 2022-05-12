module Recs.TypeInfo where

import           Control.Applicative           ((<|>),liftA2)
import           Control.Lens                  hiding (from)
import           Control.Monad.Catch           (MonadCatch,MonadMask,MonadThrow)
import           Control.Monad.Primitive       (PrimMonad(..))
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe

import qualified Data.Sequence as SQ
import           Data.Coerce
import           Data.Default
import           Data.Either                   (fromRight)
import           Data.Generics.Labels
import qualified Data.HashMap.Internal         as HM
import           Data.HashMap.Internal         (Hash)
import qualified Data.HashMap.Strict           as HMS
import           Data.Hashable
import           Data.IORef
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Kind
import           Data.Maybe                    (fromJust,fromMaybe,isJust)
import           Data.Primitive.PVar
import           Data.Proxy                    (Proxy(..))
import qualified Data.Sequence                 as SQ
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Data.Typeable                 (Typeable,typeRep,typeRepFingerprint)
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Heap   as V
import qualified Data.Vector.Algorithms.Search as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.Vector.Growable          as VR
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Primitive         as VP
import qualified Data.Vector.Unboxed           as VU
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.Word

import           GHC.Base                      (Any,IO(..),RealWorld)
import           GHC.Fingerprint               (Fingerprint(..))
import           GHC.Generics                  (Generic)
import           GHC.Stack                     (HasCallStack)
import           GHC.TypeLits

import           Unsafe.Coerce

import           Witch                         hiding (over)

import Recs.Utils

import Recs.Lib (EntityId(..), TypeId(..), Edge(..))

{-

= Garbage Collection Avoidance

GHC's non-moving collector tracks mutation in the non-moving heap via a mut list.
When an object is mutated, its contents are added to the mark queue.

== Use of Mutation

Mutation is used in a few instances:

  - When the underlying data is unboxed and needs to be safely shared across
    multiple threads without locks
  - When frequent appends are needed, either an unboxed or boxed mutable vector is used

== Goals

The goal is to avoid unnecessary GC on as much data as possible during a Gen 1
collection. Thus, minimize the amount of boxed, mutable data.

-}

data Arch = MkArch
      { stores   :: Vector Any
      , entities :: GUIOVector EntityId
      , types    :: UVector TypeId
      , edges    :: GUIOVector Edge
      }
      deriving Generic





























