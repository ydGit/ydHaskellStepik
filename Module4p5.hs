{- Module 4.5
Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том
смысле, что существуют взаимно обратные функции, преобразующие List a в [a] и обратно.
Реализуйте эти функции.
-}

data List a = Nil | Cons a (List a)
  deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

testList = Cons 'y' (Cons 'x' Nil)
----------------------------------------

data Nat = Zero | Suc Nat
  deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n
  | n == 0 = Zero
  | n > 0 = Suc (toNat (n-1))
  | otherwise = error "Must be greater than zero"

add :: Nat -> Nat -> Nat
add n m = toNat $ (fromNat n) + (fromNat m)

mul :: Nat -> Nat -> Nat
mul n m = toNat $ (fromNat n) * (fromNat m)

fac :: Nat -> Nat
fac Zero = Suc Zero
fac m@(Suc n) = toNat $ (fromNat m) * (fromNat (fac (toNat ((fromNat m) - 1))))


{- Other solutions:
This one is beautiful. I saw it before, Peano's numbers.

add :: Nat -> Nat -> Nat
add Zero x = x
add (Suc x) y = add x (Suc y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc x) y = add (mul x y) y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n1@(Suc n) = mul n1 (fac n)
-}


{-
Тип бинарных деревьев можно описать следующим образом:

data Tree a = Leaf a | Node (Tree a) (Tree a)


Реализуйте функцию height, возвращающую высоту дерева, и функцию size,
возвращающую количество узлов в дереве (и внутренних, и листьев).
Считается, что дерево, состоящее из одного листа, имеет высоту 0.
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node (Leaf _) (Leaf _)) = 1
height (Node l r) = 1 + max (height l) (height r)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = 1 + (size l) + (size r)


{-
Теперь нам нужно написать функцию avg, которая считает среднее арифметическое
всех значений в дереве. И мы хотим, чтобы эта функция осуществляла только один
проход по дереву. Это можно сделать при помощи вспомогательной функции,
возвращающей количество листьев и сумму значений в них. Реализуйте эту функцию.
-}

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1, x)
    go (Node l r) = let (cl, sl) = go l
                        (cr, sr) = go r
                    in
                      (cl+cr, sl+sr)

{-
Исправьте определение функции expand
(Найти все случаи когда она не раскрывает все подвыражения)
E.g.
GHCi> expand $ (Val 3 :+: Val 4) :*: (Val 5 :+: Val 6)
Val 3 :*: (Val 5 :+: Val 6) :+: Val 4 :*: (Val 5 :+: Val 6)

так, чтобы она, используя дистрибутивность (а также, возможно,
ассоциативность и коммутативность), всегда возвращала значение,
эквивалентное данному и являющееся суммой произведений числовых значений.


GHCi> expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
Val 1 :*: Val 4 :+: (Val 1 :*: Val 5 :+: (Val 2 :*: Val 4 :+: (Val 2 :*: Val 5 :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5))))

Примечание. Скобки в ответе могут быть расставлены по-другому или вообще
отсутствовать, поскольку сложение ассоциативно. Слагаемые могут идти в
другом порядке, поскольку сложение коммутативно.
-}

infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

testExpr1 = (Val 1 :+: Val 2 :+: Val 3) :*: Val 4
testExpr2 = (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
testExpr3 = (Val 4 :+: Val 5) :*: (Val 1 :+: Val 2 :+: Val 3)

eval :: Expr -> Int
eval (Val x) = x
eval (x :+: y) = eval x + eval y
eval (x :*: y) = eval x * eval y

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand (e1 :*: e) :+: expand (e2 :*: e)
expand (e :*: (e1 :+: e2)) = expand ((e1 :+: e2) :*: e)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = let ex = expand e1
                         ey = expand e2
                         ez = ex :*: ey
                     in
                       case ez of
                         e@((_ :+: _) :*: _) -> expand e
                         e@(_ :*: (_ :+: _)) -> expand e
                         e -> e

expand e = e
-- It took me many months to figure this one out. I am dumb.
