module TreeTraversal where

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

testTree :: Tree Char
testTree = Branch (Branch Nil 'B' (Branch Nil 'D' Nil)) 'A' (Branch (Branch Nil 'E' Nil) 'C' (Branch Nil 'F' Nil))

preOrder :: (b -> a -> b) -> b -> (Tree a) -> b
preOrder _ ini Nil = ini
preOrder f ini (Branch l n r) = preOrder f (preOrder f (f ini n) l) r

postOrder :: (b -> a -> b) -> b -> (Tree a) -> b
postOrder _ ini Nil = ini
postOrder f ini (Branch l n r) = f (postOrder f (postOrder f ini l) r) n

inOrder :: (b -> a -> b) -> b -> (Tree a) -> b
inOrder _ ini Nil = ini
inOrder f ini (Branch l n r) = inOrder f (f (inOrder f ini l) n) r

levelOrder :: (a -> b -> b) -> b -> (Tree a) -> b
levelOrder f ini t = foldr f ini $ concat $ levelOrder' t

preOrderedTree = preOrder (\xs y -> xs ++ [y]) [] testTree
postOrderedTree = postOrder (\xs y -> xs ++ [y]) [] testTree
inOrderedTree = inOrder (\xs y -> xs ++ [y]) [] testTree
levelOrderedTree = levelOrder (\y xs -> [y] ++ xs) [] testTree

-- levelOrder :: (b -> a -> b) -> b -> (Tree) ->
levelOrder' :: Tree a -> [[a]]
levelOrder' Nil = [[]]
levelOrder' (Branch Nil n Nil) = [[n], []]
levelOrder' (Branch Nil n r) = [n] : levelOrder' r
levelOrder' (Branch l n Nil) = [n] : levelOrder' l
levelOrder' (Branch l n r) = [n] : ms
  where ms = zipWith (++) (levelOrder' l) (levelOrder' r)

tree2 = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))
preOrderedTree' = preOrder (\xs y -> xs ++ [y]) []
