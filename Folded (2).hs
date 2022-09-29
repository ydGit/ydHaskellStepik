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

-- The following implementation is wrong!
instance Foldable Levelorder where
  foldr _ ini (LevelO Nil) = ini
  foldr f ini (LevelO (Branch Nil n Nil)) = f n ini
  foldr f ini (LevelO (Branch (Branch Nil ln Nil) n Nil)) = f n $ f ln ini
  foldr f ini (LevelO (Branch Nil n (Branch Nil rn Nil))) = f n $ f rn ini
  foldr f ini (LevelO (Branch (Branch Nil ln Nil) n (Branch Nil rn Nil))) = f n $ f ln $ f rn ini
  foldr f ini (LevelO (Branch (Branch llt ln lrt) n (Branch rlt rn rrt))) = f n $ f ln $ f rn $ foldr f (foldr f (foldr f (foldr f ini (LevelO rrt)) (LevelO rlt)) (LevelO lrt)) (LevelO llt)
--  foldr f ini (LevelO (Branch l n r)) = f n $ foldr f (foldr f ini (LevelO l)) (LevelO r)


levelTree = LevelO (Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch (Branch Nil 6 Nil) 5 (Branch Nil 7 Nil)))
simpleTree = Branch (Branch Nil 1 Nil) 3 (Branch Nil 4 Nil)
tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
{-
        3
      /   \
    1      4
     \
      2
-}
postorder :: Tree a -> [a]
postorder Nil = []
postorder (Branch e x d) = (++) (postorder e) ((++) (postorder d) [x])
{-
Found on the web:

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Branch x e d) = [x]++(preorder e)++(preorder d)

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Branch x e d) = (inorder e)++[x]++(inorder d)

-- MY COMMENT: Indeed it works.
postorder :: Tree a -> [a]
postorder Empty = []
postorder (Branch x e d) = (postorder e)++(postorder d)++[x]
-}

-- infixr 9 |.|
newtype Cmps f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

c1 = Cmps [Nothing, Just 2, Just 3]
c2 = Cmps [[1,2], [], [3,4,5,6,7]]

-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- h :: (a -> m)
-- c :: t (t a)
instance (Foldable g, Foldable f) => Foldable (Cmps f g) where
  foldMap h c = let c' = getCmps c in
    (foldMap . foldMap) h $ c'
