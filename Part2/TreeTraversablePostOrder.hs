module TreeTraversablePostOrder where

import Data.Traversable (foldMapDefault)

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)


instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch tl x tr) = Branch (fmap f tl) (f x) (fmap f tr)

instance Applicative Tree where
  pure x = Branch Nil x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Branch ftl f ftr) <*> (Branch vtl v vtr) = Branch (ftl <*> vtl) (f v) (ftr <*> vtr)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  sequenceA Nil = pure Nil
  sequenceA (Branch tl x tr) = pure postOrderBranch <*> (sequenceA tl) <*> (sequenceA tr) <*> x
                               where postOrderBranch tl tr n = Branch tl n tr


tr1 = (Branch (Branch Nil 1 Nil) 3 Nil)
tr2 = (Branch (Branch Nil 1 Nil) 2 Nil)
tr3 = Branch (Branch Nil [1,2] Nil) [3] Nil

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
