module Recs.Arch (Edge, Archetype, Archetypes) where

import           Control.Applicative           ((<|>),liftA2)
import           Control.Lens                  hiding (from)
import           Control.Monad.Catch           (MonadCatch,MonadMask,MonadThrow)
import           Control.Monad.Primitive       (PrimMonad(..))
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe

import           Data.Bits
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

import           Recs.Core
import           Recs.Utils
import           Recs.Utils

import           Unsafe.Coerce

import           Witch                         hiding (over)

-- | An edge in the Archetype graph.
data Edge =
  MkEdge
  { add    :: !(Maybe ArchId)
  , remove :: !(Maybe ArchId)
  }
  deriving (Generic,Show)

data Archetype =
  MkArch
  { components :: Vector Any
  , entities   :: GUIOVector EntityId
  , types      :: UVector TypeId
  , edges      :: GUIOVector Edge
  }
  deriving Generic

newtype Archetypes = MkArchetypes (GIOVector Archetype)
  deriving Generic


























