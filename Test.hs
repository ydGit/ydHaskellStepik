module Test where

import Data.Function

infixl 6 |-|

x |-| y = abs (x-y)

fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 0 = fibonacci (n-1) + fibonacci (n-2)
            | n < 0 = fibonacci (n+2) - fibonacci (n+1)

factorial5 n  | n >= 0 = helper 1 n
              | otherwise = error "agr must be >= 0"

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

fibAccum n | n > 0 = helperPos 0 1 n
           | otherwise = helperNeg 0 1 n

helperPos f0 f1 0 = f0
helperPos f0 f1 n = helperPos f1 (f0 + f1) (n - 1)

helperNeg f0 f1 0 = f0
helperNeg f0 f1 n = helperNeg (f1 - f0) f0 (n + 1)

seqA :: Integer -> Integer
seqA n = if (n == 0) || (n == 1) || (n == 2) then (n + 1) else
           let helper a0 a1 a2 n =
                 if n == 2 then a2 else helper a1 a2 (a2+a1-2*a0) (n - 1)
           in helper 1 2 3 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = (sum y, len y)
  where
    y = intToList (abs x)
    intToList z = if z == 0 then [] else (z `mod` 10):(intToList (z `div` 10))
    len [] = 0
    len (z:zs) = 1 + len zs


integration :: (Double -> Double) -> Double -> Double -> Double
integration f x y | x == y = 0
                  | otherwise = step * ( (f x + f y)/2  + sumf f x step n )
  where n = 1000
        step = (y - x)/n
        sumf f x step k = if k == 1 then 0 else f (x + step * (k - 1)) + sumf f x step (k - 1)


-- multSecond = g `on` h
-- g = (*)
-- h = snd

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

doItYourself = f . g . h
f = max 42
g = (^3)
h = logBase 2

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (x, y) = "(" ++ toString x ++ "," ++ toString y ++ ")"


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | doesEnrageGork x && doesEnrageMork x = stomp $ stab x
                  | doesEnrageGork x = stab x
                  | doesEnrageMork x = stomp x
                  | otherwise = x

a = 127.2
b = 24.1
c = 20.1
d = 2
ip = show a ++ show b ++ show c ++ show d

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  spred :: a -> a
  ssucc x = if x == maxBound then minBound else succ x
  spred x = if x == minBound then maxBound else pred x

instance SafeEnum Bool

avg :: Int -> Int -> Int -> Double
avg x y z = (x' + y' + z')/3.0
  where x' = fromIntegral x :: Double
        y' = fromIntegral y :: Double
        z' = fromIntegral z :: Double


foo a = a
bar = const foo
baz x = const True
quux = let x = x in x
corge = "Sorry, my value was changed"
grault x 0 = x
grault x y = x
garply = grault 'q'
waldo = foo
