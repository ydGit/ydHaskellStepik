module Traversable where

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (g . f) (pure [])
  where g = (\x y -> pure (:) <*> x <*> y)
