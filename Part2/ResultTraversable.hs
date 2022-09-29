module ResultTraversable where

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
  fmap f (Ok a) = Ok (f a)
  fmap f (Error s) = Error s

instance Applicative Result where
  pure x = Ok x
  (Ok f) <*> (Ok x) = Ok (f x)
  (Ok f) <*> (Error s) = Error s
  (Error s) <*> (Ok x) = Error s
  (Error sf) <*> (Error sx) = Error (sf ++ " and " ++ sx)

instance Foldable Result where
  foldr f ini (Ok x) = f x ini
  foldr _ ini (Error s) = ini

instance Traversable Result where
  traverse f (Ok x) = pure Ok <*> f x
  traverse f (Error s) = pure Error <*> pure s
