{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Access where

import Prelude hiding (Read)
import Data.Kind
import GHC.TypeLits

class Read i l

instance (ContainsEq i l ~ 'True) => Read i l

class Write i l

instance (ContainsEq i l ~ 'True) => Write i l

class Access i l

type family Merge a b where
  Merge (a ': as) b = a ': Merge as b
  Merge '[] b = b

type family NEq a b where
  NEq x x = 'False
  NEq x y = 'True

type family And a b where
  And 'True 'False = 'False
  And 'False 'True = 'False
  And 'False 'False = 'False
  And 'True 'True = 'True

type family Or a b where
  Or 'True 'False = 'True
  Or 'False 'True = 'True
  Or 'False 'False = 'False
  Or 'True 'True = 'True

type family AccessesReads l r where
  AccessesReads (a ': as) r = (Read a r, AccessesReads as r)
  AccessesReads _ _ = ()

type family AccessesWrites l w where
  AccessesWrites (a ': as) w = (Write a w, AccessesWrites as w)
  AccessesWrites _ _ = ()

type family Accesses reads writes r w where
  Accesses reads writes r w = (AccessesReads reads r, AccessesWrites writes w)

type family If c a b where
  If 'True a _ = a
  If 'False _ b = b

type family ContainsEq a b :: Bool where
  ContainsEq a '[] = 'False
  ContainsEq a (a ': as) = 'True
  ContainsEq a (b ': bs) = ContainsEq a bs

type family Conflicts a b :: [Type] where
  Conflicts '[] _ = '[]
  Conflicts a '[] = '[]
  Conflicts (a ': as) b = If (ContainsEq a b) (a ': Conflicts as b) (Conflicts as b)

type family ParallelAccess r1 r2 w1 w2 where
  ParallelAccess r1 r2 w1 w2 = If (Or (NEq (Conflicts r1 w2) '[]) (NEq (Conflicts r2 w1) '[]))
                               (TypeError ('Text "System dependencies conflict!")) -- TODO
                               (Merge w1 w2)
