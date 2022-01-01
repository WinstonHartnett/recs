module Types where

-----------------------------------------------------------------------------------------
-- Typelevel Programming
-----------------------------------------------------------------------------------------
type family IsTuple t where
  IsTuple (a, b) = True
  IsTuple (a, b, c) = True
  IsTuple (a, b, c, d) = True
  IsTuple _ = False

type family (l :: [Type]) ++  (m :: [Type]) :: [Type] where
  (a : as) ++ b = a : (as ++ b)
  '[] ++ b = b

type family Equals a b where
  Equals a a = True
  Equals _ _ = False

type family If c a b where
  If True a _ = a
  If False _ b = b

type family In l e where
  In (a : as) a = True
  In (a : as) e = In as e
  In '[] e = False

type family TupleToList t where
  TupleToList (a, b) = '[a, b]
  TupleToList (a, b, c) = '[a, b, c]
  TupleToList (a, b, c, d) = '[a, b, c, d]

type family ListToTuple l where
  ListToTuple '[a, b] = (a, b)
  ListToTuple '[a, b, c] = (a, b, c)
  ListToTuple '[a, b, c, d] = (a, b, c, d)
  ListToTuple '[a] = a

  -- TupleToList a = '[a]
type family Map f l where
  Map f (a : as) = f a : Map f as
  Map _ '[] = '[]

type family IsList l where
  IsList (a : _) = True
  IsList '[] = True
  IsList _ = False

type family Or a b where
  Or True False = True
  Or False True = True
  Or True True = True
  Or False False = False
