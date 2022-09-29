module OddContainer where

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)


instance Functor OddC where
  fmap f (Un x) = Un (f x)
  fmap f (Bi x y z) = Bi (f x) (f y) (fmap f z)


instance Foldable OddC where
  -- foldr :: Foldable OddC => (a -> b -> b) -> b -> OddC a -> b
  -- f :: (a -> b -> b)
  -- ini :: b
  foldr f ini (Un x) = f x ini
  foldr f ini (Bi x y z) = let u = foldr f ini z in f x (f y u)

instance Traversable OddC where
  -- traverse :: (Traversable OddC, Applicative f) => (a -> f b) -> OddC a -> f (OddC b)
  -- sequenceA :: (Traversable OddC, Applicative f) => OddC (f a) -> f (OddC a)
  -- g :: (a -> f b)
  traverse g (Un x) = Un <$> (g x)
  traverse g (Bi x y z) = (pure Bi) <*> (g x) <*> (g y) <*> (traverse g z)



cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3
