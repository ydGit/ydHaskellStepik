{-# LANGUAGE TypeOperators #-}
module Typecomp where

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)
instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  (<*>) = undefined


-- that is f |.| g is a type constructor that can be applied to a
-- type of kind * like (f |.| g) a
-- Here are a couple of examples:

type A   = ((,) Integer |.| (,) Char) Bool
{-
Here f = (,) Integer
     g = (,) Char
then h = f |.| g and h a = f (g a), where a = Bool. We thus have
h Bool = f ( (,) Char Bool ) = f (Char, Bool) = (,) Integer (Char, Bool) = (Integer, (Char, Bool))
A = Cmps (Integer, (Char, Bool))
-}

type B t = ((,,) Bool (t -> t) |.| Either String) Int
{-
Here f = (,,) Bool (t->t) = (Bool, (t->t),)
     g = Either String
     a = Int
then g a = Either String Int
     f (g a) = (Bool, (t->t), Either String Int)
-}

type C   = (|.|) ((->) Bool) ((->) Integer) Integer
{-
Here f = ((->) Bool) = (Bool -> )
     g = ((->) Integer) = (Integer -> )
     a = Integer
     g a = (Integer -> Integer)
     f (g a) = (Bool -> (Integer -> Integer))
-}

a :: A
a = Cmps (4, ('2', True))

b :: B t
b = Cmps (False, id, Right 42)

c :: C
c  = Cmps (\ x -> if x then \y -> y+2 else \y -> y * 2)

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
  deriving (Eq,Show)

{-
fmap :: (a -> b) -> Cmps3 f g h a -> Cmps3 f g h b
-}
instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap fun (Cmps3 x) = Cmps3 $ fmap (fmap (fmap fun)) x
{-
x :: f (g (h a))
we want to turn this into f (g (h b)) with the help of fun :: (a->b)
if we can find
phi :: h a -> h b, then
we can buid
psi :: g (h a) -> g (h b) by fmap phi g
and we would do
fmap psi x = fmap (fmap phi g) x = fmap (fmap (fmap fun) g)
-}

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3  x = fmap getCmps $ getCmps x

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 x = fmap (fmap getCmps) (fmap getCmps $ getCmps x)
