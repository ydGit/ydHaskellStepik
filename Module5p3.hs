-- Started March 16, 2018
module Stepik5p2 where
{-
Если некоторый тип является представителем класса Monad, то его можно сделать
представителем класса Functor, используя функцию return и оператор >>=.
Причём, это можно сделать даже не зная, как данный тип устроен.

Пусть вам дан тип
data SomeType a = ...
и он является представителем класса Monad. Сделайте его представителем класса Functor
-}
{- My correct solution:

instance Functor SomeType where
  fmap f x = x >>= return . f
-}

data Log a = Log [String] a deriving Show

returnLog :: a -> Log a
returnLog x = Log [] x

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log ss x) f = let (Log ss' y) = f x
                           in Log (ss' ++ ss ++ ss') y
-- helper functions for testing
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s = \x -> Log [s] (f x)
add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

instance Functor Log where
  fmap = undefined

instance Applicative Log where
  pure = undefined
  (<*>) = undefined

instance Monad Log where
    return = returnLog

    (>>=) = bindLog

testM = Log ["hello"] 3
