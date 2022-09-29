module Module3 where

import Data.Char (isDigit, isLower)
import Data.List (foldl')

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements = \x y xs -> x : y : xs


nTimes:: a -> Int -> [a]
nTimes x 0 = []
nTimes x n = x : nTimes x (n - 1)

sndHead = snd . head
{--
sndHead ((_, x) : _) = x
--}

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
                | otherwise = oddsOnly xs
{--
GHCi> oddsOnly [2,5,7,10,11,12]
[5,7,11]
--}


-- reverse l = rev l [] where
--   rev [] a = a
--   rev (x:xs) a = rev xs (x:a)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- TODO: Make shorter/more elegant
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs
sum3 (x:xs) (y:ys) [] = (x + y) : sum3 xs ys []
sum3 (x:xs) [] (z:zs) = (x + z) : sum3 xs [] zs
sum3 [] (y:ys) (z:zs) = (y + z) : sum3 [] ys zs
sum3 (x:xs) [] [] = x : sum3 xs [] []
sum3 [] (y:ys) [] = y : sum3 [] ys []
sum3 [] [] (z:zs) = z : sum3 [] [] zs
sum3 _ _ _ = []


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs) = let (y:ys) = groupElems xs
                    in if (x == head y)
                       then (x : y) : ys
                       else [x] : (y: ys)
{--
GHCi> groupElems []
[]
GHCi> groupElems [1,2]
[[1],[2]]
GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]
GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]
--}

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p q = filter r where r x = p x || q x

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = let ys = filter (<=x) xs
                   zs = filter (>x) xs
               in (qsort ys) ++ [x] ++ (qsort zs)

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x * x, x * x * x])
-- squares'n'cubes [] = []
-- squares'n'cubes (x:xs) = undefined

-- squares'n'cubes [3,4,5]
-- [9,27,16,64,25,125]

-- Module 3.2, Exercise 8
-- Insert an element at every place in the list
multInsert :: a -> [a] -> [[a]]
multInsert x [] = [[x]]
multInsert x [y] = [[x, y], [y, x]]
multInsert x (y:z:zs) = [(x:y:z:zs), (y:x:z:zs)]++(map ([y, z]++) $ multInsert x zs)
-- permulations using recursion and map+concat
perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms [x, y] = [[x, y], [y, x]]
perms (x:xs) = let ys = perms xs
                 in concatMap (multInsert x) ys

-- Module 3.2 Exercise 10
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower ) . words

-- Module 3.2 Exercise 12
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> maximum [x, y, z])

-- Module 3.3 Exercise 3
-- Реализуйте c использованием функции zipWith функцию fibStream, возвращающую
-- бесконечный список чисел Фибоначчи.
-- 1, 1, 2, 3, 5, 8, 13, 21, 34
-- 1, 1,  2 = 1 + 1, 3 = 2 + 1, 5 = 2 + 3 ...
fibStream :: [Integer]
fibStream = [0, 1] ++ (zipWith (+) fibStream (tail fibStream))

-- Module 3.3 Exercise 5
-- Предположим, что функция repeat, была бы определена следующим образом:
-- repeat = iterate repeatHelper
-- определите, как должна выглядеть функция repeatHelper.
repeatHelper = id

-- data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
data Odd = Odd Integer deriving (Eq,Show)

-- Use the fact that odd_n = 2*n+1, n = 0, +/-1, +/-2, ...
-- instance Enum Odd where
--   fromEnum (Odd n) = div (fromIntegral n) 2
--   toEnum n = Odd (toInteger $ (2 * n + 1))

instance Enum Odd where
  succ (Odd n) = Odd (n + 2)
  pred (Odd n) = Odd (n - 2)
  enumFrom n = n : enumFrom (succ n)
  enumFromThen (Odd n) (Odd k) = (Odd n) : enumFromThen (Odd k) (Odd (2 * k - n))
  enumFromTo (Odd n) (Odd k) = let xs = enumFrom (Odd n)
                                   p (Odd m) = m <= k
                               in takeWhile p xs
  enumFromThenTo (Odd n) (Odd k) (Odd l) = let xs = enumFromThen (Odd n) (Odd k)
                                               p (Odd m) = m <= l
                                               p' (Odd m) = m >= l
                                           in if k > n then takeWhile p xs else takeWhile p' xs
  toEnum n = Odd (toInteger $ (2 * n + 1))
  fromEnum (Odd n) | odd n = div (fromIntegral n) 2
                   | otherwise = error "Argument must be odd"
-- CORRECT: Only 9% of correct solutions. Solved from 6th attempt!

-- Some tests from another user
test0 = succ (Odd 1) == (Odd 3)
test1 = pred (Odd 3) == (Odd 1)
-- enumFrom
test2 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- enumFromTo
-- -- По возрастанию
test3 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test4 = (take 3 $ [Odd 7..Odd 1]) == []
-- enumFromThen
-- -- По возрастанию
test5 = (take 3 $ [Odd 1, Odd 3 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test6 = (take 3 $ [Odd 3, Odd 1 ..]) == [Odd 3,Odd 1,Odd (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 =([Odd 1, Odd 5 .. Odd 7]) == [Odd 1,Odd 5]
-- -- По убыванию
test8 =([Odd 7, Odd 5 .. Odd 1]) == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- x1 < x3 && x1 > x2
test9 =([Odd 7, Odd 5 .. Odd 11]) == []
-- -- x1 > x3 && x1 < x2
test10 =([Odd 3, Odd 5 .. Odd 1]) == []

allTests = zip [0..] [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]

{--
Module 3, Exercise 3.9
Пусть есть список положительных достоинств монет coins, отсортированный по возрастанию.
Воспользовавшись механизмом генераторов списков, напишите функцию change, которая разбивает
переданную ей положительную сумму денег на монеты достоинств из списка coins всеми
возможными способами. Например, если coins = [2, 3, 7]:
GHCi> change 7
[[2,2,3],[2,3,2],[3,2,2],[7]]
Примечание. Порядок монет в каждом разбиении имеет значение, то есть наборы
[2,2,3] и [2,3,2] — различаются.
Список coins определять не надо.

coins = [2, 3, 7]
Notes
[[x, y, z] | x <- coins, y <- coins, z <- coins]
--}


--coins = [2, 3, 7]


--change s = [f s c | c <- coins]
-- change n = [map (div n) coins]
-- change n = let x = head coins
--                m = n `div` x
--            in replicate m coins

coins :: (Num a) => [a]
coins = [2, 3, 7]

-- Need to return change 1 = [] for coins = [2, 3, 7]
-- This solution I found after about a couple of months of contemplation.
-- It looks a bit like a hack, forced by the requirement to return [] for change 1.
-- Otherwise, if [[]] is acceptable, then the 'let' construction is not necessary
mychange :: (Ord a, Num a) => [a] -> a -> [[a]]
mychange cns@(c:cs) s
  | s < c = []
  | otherwise = [(x:xs) | x <- cns, xs <- (let ys = mychange cns (s - x) in if ys /= [] then ys else [[]]), sum (x:xs) == s]

change :: (Ord a, Num a) => a -> [[a]]
change = mychange coins


-- Find sum of positive elements of the list
sumPositiveSquares :: [Integer] -> Integer
-- sumPositiveSquares [] = 0
-- sumPositiveSquares (x:xs)
--   | x > 0 = x * x + sumPositiveSquares xs
--   | otherwise = sumPositiveSquares xs

-- now in terms of foldr
sumPositiveSquares = foldr f 0 where f x y = if x > 0 then x^2 + y else y

lengthList :: [a] -> Int
lengthList = foldr (\_ y -> y + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

--Какой функции стандартной библиотеки эквивалентно выражение foldr const undefined?
{-
Given that
foldr f ini e1 : e2 : e3 : ... eN : []
is
e1 `f` (e2 `f` (e3 `f` ... (eN `f` ini))))
we have
e1 `const` (e2 `const` (e3 `const` ... (eN `const` undefined)
and
const x y = x
we have
eN `const` undefied = eN
...
e3 `const` eN = e3
e1 `const` e2 = e1
therefore it is head!
-}

-- Module 3.5
-- Helper function: cumulative average
f :: Double -> (Double, Double) -> (Double, Double)
f x (y, n) = ( (n * y + x) / (n + 1), n + 1)

meanList :: [Double] -> Double
meanList = fst.foldr f (0, 0)

{-
Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает из списка элементы,
стоящие на нечетных местах, оставляя только четные.
GHCi> evenOnly [1..10]
[2,4,6,8,10]
GHCi> evenOnly ['a'..'z']
"bdfhjlnprtvxz"
-}

evenOnly :: [a] -> [a]
evenOnly  = fst.foldl g ([], False) where
  g (xs, e) x
    | e = (xs ++ [x], False)
    | otherwise = (xs, True)

{-
Попробуйте добиться того, чтобы реализованная вами в прошлом задании функция
evenOnly позволяла работать и с бесконечными списками.
То есть, например, запрос на первые три элемента бесконечного списка, возвращаемого
этой функцией, примененной к списку всех натуральных чисел, должен завершаться:

GHCi> take 3 (evenOnly [1..])
[2,4,6]
-}

-- version that works with the infinite lists
evenOnlyInf :: [a] -> [a]
evenOnlyInf (_:y:ys) = y : evenOnlyInf ys
evenOnlyInf _ = []

{-
Staff solution
evenOnly :: [a] -> [a]
evenOnly = snd . foldr (\x ~(xs, ys) -> (x : ys, xs)) ([], [])
Wow! Alternating elements of the pair! Brilliant!
-}
evenOnly2 :: [a] -> [a]
evenOnly2 = snd.foldr g ([], []) where
  g x ~(xs, ys) = (x:ys, xs)

{-
Another solution w/o lazy pattern. Very nice
evenOnly :: [a] -> [a]
evenOnly = snd . foldr f ([],[]) where
    f x p = (x:snd p, fst p)
-}

{-
Module 3.6
Напишите реализацию функции, возвращающей последний элемент списка, через foldl1.
-}
lastElem :: [a] -> a
lastElem = foldl1 (flip const)

{-
Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном
порядке список символов, попадающих в заданный парой диапазон. Попадание символа
x в диапазон пары (a,b) означает, что x >= a и x <= b

GHCi> revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"
-}
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
  helper (Just (x, ini')) = x : unfoldr f ini'
  helper Nothing = []

revRange :: (Char,Char) -> [Char]
revRange (lower, upper) = unfoldr g upper
  where g c = if c >= lower then Just (c, pred c) else Nothing
{- Notes:
a = Char
ini = Char
g :: Char -> Maybe (Char, Char)
g c = if c >= lower then Just c else Nothing
-}
