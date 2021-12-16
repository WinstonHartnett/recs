{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Recs.TypeLevel where

import Data.Kind
import Prelude

-- TODO Swap this out w/ an actual library

type family TupleToList t where
  TupleToList (a, b) = '[a, b]
  TupleToList (a, b, c) = '[a, b, c]
  TupleToList (a, b, c, d) = '[a, b, c, d]
  TupleToList (a, b, c, d, e) = '[a, b, c, d, e]
  TupleToList a = '[a]

type family ToTuple l where
  ToTuple (a ': as) = (a, ToTuple as)
  ToTuple '[] = ()

type family ToTupleC l :: Constraint where
  ToTupleC (a ': as) = (a, ToTupleC as)
  ToTupleC '[] = ()

type family (++) a b where
  (a ': as) ++ b = a ': (as ++ b)
  '[] ++ b = b

type family Map (f :: j -> k) l :: [k] where
  Map f (a ': as) = f a ': Map f as
  Map _ '[] = '[]

type family ToConstraint (l :: [Constraint]) where
  ToConstraint (a ': as) = (a, ToConstraint as)
  ToConstraint '[] = ()

type family If c a b where
  If 'True a _ = a
  If 'False _ b = b

type family IfC c a b :: Constraint where
  IfC 'True a _ = a
  IfC 'False _ b = b

type family Equal a b where
  Equal a a = 'True
  Equal a b = 'False

type family Contains e l :: Bool where
  Contains e (a ': as) = If (Equal e a) 'True (Contains e as)
  Contains e '[] = 'False

type family Snd x where
  Snd '(_, b) = b

type family Fst x where
  Fst '(a, _) = a

type family IsTuple t where
  IsTuple (a, b) = 'True
  IsTuple (a, b, c) = 'True
  IsTuple (a, b, c, d) = 'True
  IsTuple (a, b, c, d, e) = 'True
  IsTuple a = 'False

type family Subset a b :: Bool where
  Subset (a ': as) x = If (Contains a x) (Subset as x) 'False
  Subset '[] _ = 'True




