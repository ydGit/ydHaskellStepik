module Folded where

data Triple a = Tr a a a  deriving (Eq,Show)
{-
GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
-}
instance Foldable Triple where
  foldl f ini (Tr x y z) = f (f (f ini x) y) z
  foldr f ini (Tr x y z) = f x (f y (f z ini))

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldr _ ini Nil = ini
  foldr f ini (Branch l n r) = foldr f (f n (foldr f ini r)) l

treeToList :: Tree a -> [a]
treeToList = foldr (:) []

newtype Preorder a   = PreO (Tree a) deriving (Eq, Show)
instance Foldable Preorder where
  foldr _ ini (PreO Nil) = ini
  foldr f ini (PreO (Branch Nil n Nil)) = f n ini
  foldr f ini (PreO (Branch (Branch Nil ln Nil) n Nil)) = f n (f ln ini)
  foldr f ini (PreO (Branch Nil n (Branch Nil rn Nil))) = f n (f rn ini)
  foldr f ini (PreO (Branch (Branch Nil ln Nil) n (Branch Nil rn Nil))) = f n (f ln (f rn ini))
  foldr f ini (PreO (Branch (Branch llt ln lrt) n (Branch rlt rn rrt))) = f n (f ln (foldr f (foldr f (f rn (foldr f (foldr f ini (PreO rrt)) (PreO rlt))) (PreO lrt)) (PreO llt)))


newtype Postorder a = PostO  (Tree a) deriving (Eq, Show)
instance Foldable Postorder where
  foldr _ ini (PostO Nil) = ini
  foldr f ini (PostO (Branch Nil n Nil)) = f n ini
  foldr f ini (PostO (Branch (Branch Nil ln Nil) n Nil)) = f ln (f n ini)
  foldr f ini (PostO (Branch Nil n (Branch Nil rn Nil))) = f rn (f n ini)
  foldr f ini (PostO (Branch (Branch Nil ln Nil) n (Branch Nil rn Nil))) = f ln (f rn (f n ini))
  foldr f ini (PostO (Branch (Branch llt ln lrt) n (Branch rlt rn rrt))) = foldr f (foldr f (f ln (foldr f (foldr f (f rn (f n ini)) (PostO rrt)) (PostO rlt))) (PostO lrt)) (PostO llt)

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

levelOrder' :: Tree a -> [[a]]
levelOrder' Nil = [[]]
levelOrder' (Branch Nil n Nil) = [[n], []]
levelOrder' (Branch Nil n r) = [n] : levelOrder' r
levelOrder' (Branch l n Nil) = [n] : levelOrder' l
levelOrder' (Branch l n r) = [n] : ms
  where ms = zipWith (++) (levelOrder' l) (levelOrder' r)

instance Foldable Levelorder where
  foldr f ini (LevelO t) = foldr f ini $ concat $ levelOrder' t

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

tree2 = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))

{-
Yes, Yes, Yes, Yes!!!!
I did it, finally, after so many months
-}
