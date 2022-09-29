module Part2 where

import Control.Applicative

newtype Arr e1 a = Arr { getArr :: e1 -> a }
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

{-
instance Functor (Arr2 e1 e2) where
  fmap f g =  (\x-> getArr2 f (g x))
-}


instance Functor (Arr e1) where
  fmap f g = Arr (\x -> f $ (getArr g) x)

instance Functor (Arr2 e1 e2) where
  fmap f g = Arr2 (\x y -> f $ (getArr2 g) x y)

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\e1 e2 -> x)
  (<*>) f g = Arr2 (\e1 e2 -> (f' e1 e2) (g' e1 e2))
    where g' = getArr2 g
          f' = getArr2 f

instance Functor (Arr3 e1 e2 e3) where
  fmap f g = Arr3 (\x y z -> f $ (getArr3 g) x y z)

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\e1 e2 e3 -> x)
  (<*>) f g = Arr3 (\e1 e2 e3 -> (f' e1 e2 e3) (g' e1 e2 e3))
    where g' = getArr3 g
          f' = getArr3 f


data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
  pure x = Tr x x x
  (Tr fx fy fz) <*> (Tr x y z) = Tr (fx x) (fy y) (fz z)

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f xs = getZipList ( f <$> ys )
  where ys = ZipList xs

(>*<) :: [(a->b)] -> [a] -> [b]
(>*<) f xs = getZipList ( g <*> ys )
  where g = ZipList f
        ys = ZipList xs

infixl 4 >$<
infixl 4 >*<

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> ("<-"++show x++"/", x) <*> divideList' xs
-- ("<-3.0/<-4.0/<-5.0/1.0",3.75)

foo 0 x = x
foo n x = let x' = foo (n - 1) (x + 1)
          in x' `seq` x'

foo' 0 x = x
foo' n x = let x' = foo' (n - 1) (x + 1)
          in x'

bar 0 f = f
bar x f = let f' = \a -> f (x + a)
              x' = x - 1
          in f' `seq` x' `seq` bar x' f'

bar' 0 f = f
bar' x f = let f' = \a -> f (x + a)
               x' = x - 1
           in bar' x' f'

baz 0 (x, y) = x + y
baz n (x, y) = let x' = x + 1
                   y' = y - 1
                   p  = (x', y')
                   n' = n - 1
               in p `seq` n' `seq` baz n' p

baz' 0 (x, y) = x + y
baz' n (x, y) = let x' = x + 1
                    y' = y - 1
                    p  = (x', y')
                    n' = n - 1
                in baz' n' p

quux 0 (x, y) = x + y
quux n (x, y) = let x' = x + 1
                    y' = y - 1
                    p  = (x', y')
                    n' = n - 1
                in x' `seq` y' `seq` n' `seq` quux n' p

quux' 0 (x, y) = x + y
quux' n (x, y) = let x' = x + 1
                     y' = y - 1
                     p  = (x', y')
                     n' = n - 1
                 in quux' n' p



{-
These functions are to be used to construct
Cantor's mapping of a plane into a line.
-}
dropWhole :: Double -> Double
dropWhole x = x - y
  where y = fromIntegral $ floor x

getNthDigit :: Int -> Double -> Int
getNthDigit n x = floor $ y * 10
  where y = dropWhole $ x * 10^(n-1)


infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)
