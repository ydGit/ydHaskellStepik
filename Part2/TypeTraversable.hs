{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeTraversable where

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance Functor ((|.|) f g) where
  fmap = undefined

instance Applicative ((|.|) f g) where
  pure = undefined
  (<*>) = undefined

instance Foldable ((|.|) f g) where
  foldr = undefined

{-аргументы композиции являются представителями Traversable-}

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse h (Cmps x) = pure Cmps <*> traverse phi x
                        where phi = traverse h

{-
A video from type-level composition helped me a lot to understand what is required!
-}

{-
in general
traverse :: (a -> k b) -> t a -> k (t b)

our goal is k (f d) where d :: g b
from (f c) where c :: g a
given
x :: f (g a)


phi :: (g a -> k (g b))

traverse phi :: t (g a) -> k (t (g b)) where t :: f

when applied to x :: f (g a) this will yield

k (f (g b))

Now, how can we construct phi :: (g a) -> k (g b)

we have traverse and h and x. We already used x. Let's look at

h :: a -> k b

traverse h :: t a -> k (t b) where t :: g

traverse h :: g a -> k (g b)
-}
