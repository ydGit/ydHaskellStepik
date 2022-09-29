module Scratches where

data Pair a b = Pair a b

type Comment = String

-- value with an added context in form of a comment
newtype Commented a = Commented {getCommented :: Pair Comment a}

instance Functor Commented where
  fmap f (Commented (Pair c v)) = Commented $ Pair c (f v)

instance Applicative Commented where
  pure v = Commented $ Pair "" v
  (Commented (Pair cf f)) <*> (Commented (Pair cv v)) = Commented $ Pair c' (f v)
    where c' = cf ++ " applied to " ++ cv
--------------------------------------------------

-- value with an added context in form of a source/cause
newtype CausedBy b a = CausedBy {getCausedBy :: Pair b a}

instance Functor (CausedBy b) where
  fmap f (CausedBy (Pair x y)) = CausedBy $ Pair x (f y)


data Function c a = Function {getFunction :: c -> a}

{-
c is the context. The table/database/source/dictionary
You can lookup values there, as well as functions. From _the_ _same_ context.

In a sense, function _is_ a table (a set of pairs)

in -> out
---------
c1 -> a1
c2 -> a2
c3 -> a3
...
cN -> aN
--------

-}


instance Functor (Function a) where
  fmap f (Function g) = Function $ f . g

instance Applicative (Function c) where
  pure v = Function $ const v
  (Function f) <*> (Function v) = Function $ \x -> (f x) (v x)

instance Monad (Function c) where
  return = pure
  (Function v) >>= k = Function $ \x -> (getFunction . k) (v x) x


-- Now need to clearly understand the semantics of function as monad
-- In a sense,
