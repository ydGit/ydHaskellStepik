module ListExcept where

import Control.Monad.Trans.Except
import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus), guard, msum)

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)



infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
xs !!! n
  | n < 0 = except . Left $ ErrNegativeIndex
  | otherwise = let xs' = drop n xs
                in
                  case xs' of
                    [] -> except . Left $ ErrIndexTooLarge n
                    (x:_) -> except . Right $ x


{- Interestingly, the solution using length function takes too much memory
  for some cases (infinite list !!!! -}

{-
GHCi> runExcept $ [1..100] !!! 5
Right 6
GHCi> (!!!!) xs n = runExcept $ xs !!! n
GHCi> [1,2,3] !!!! 0
Right 1
GHCi> [1,2,3] !!!! 42
Left (ErrIndexTooLarge 42)
GHCi> [1,2,3] !!!! (-3)
Left ErrNegativeIndex
-}


data ReadError = EmptyInput | NoParse String
  deriving Show


tryRead :: Read a => String -> Except ReadError a
tryRead [] = except $ Left EmptyInput
tryRead s = except $ case (reads s :: (Read a) => [(a, String)]) of
                       [(v, "")] -> Right v -- fully consumed string
                       otherwise -> Left (NoParse s)


data SumError = SumError Int ReadError
  deriving Show

-- use tryRead to implement summation with exceptions
-- trySum :: [String] -> Except SumError Integer
-- trySum = foldl f (0, 1) where
--   f = undefined


sum1, sum2, sum3 :: [Except ReadError Int]
sum1 = map tryRead ["10", "20", "30"]
sum2 = map tryRead ["10", "20", ""]
sum3 = map tryRead ["10", "two", "30"]

{-
GHCi> runExcept $ trySum ["10", "20", "30"]
Right 60
GHCi> runExcept $ trySum ["10", "20", ""]
Left (SumError 3 EmptyInput)
GHCi> runExcept $ trySum ["10", "two", "30"]
Left (SumError 2 (NoParse "two"))

-- Hint: use
withExcept :: (e -> e') -> Except e a -> Except e' a 


-}

-- version without using `withExcept`
-- it fails with the infinite list
trySum' :: [String] -> Except SumError Integer
-- trySum [] = except (Left $ SumError 0 EmptyInput)
trySum' xs = foldr f ini xs' where
  ini = except . Right $ 0
  xs' = zip [1..] xs
  f :: (Integer, String) -> Except SumError Integer -> Except SumError Integer
  f (n, s) exc = except $ case (runExcept exc) of
                            (Left err) -> (Left err)
                            (Right v) -> case (runExcept $ tryRead s) of
                                           (Right u) -> Right $ u + v
                                           (Left err) -> Left $ SumError (fromInteger n) err
{-
foldr :: (a -> b -> b) -> b -> t a -> b

In this context

b = Except Sum Error Integer
a = (Integer, String)
t = []
-}

-- tryRead :: Read a => String -> Except ReadError a
-- This version worked
trySum :: [String] -> Except SumError Integer
trySum xs = let ys = tryRead <$> xs :: [Except ReadError Integer]
                zs :: [Except SumError Integer]
                zs = [withExcept (SumError n) exc | (n, exc) <- zip [1..] ys]
                process sum [] = except $ Right sum
                process sum (e:es) = case (runExcept e) of
                                        (Left _) -> e
                                        (Right v) -> process (sum + v) es
           in
             process 0 zs
             

f :: Except ReadError Int -> Except SumError Int
f e = case runExcept e of
        (Left EmptyInput) -> undefined
        (Left (NoParse s)) -> undefined
        (Right v) -> except $ Right v

-- data ReadError = EmptyInput | NoParse String
-- data SumError = SumError Int ReadError
g :: Int -> ReadError -> SumError
g = SumError

----------------------------------------
newtype SimpleError = Simple { getSimple :: String } 
  deriving (Eq, Show)


instance Semigroup SimpleError where
  Simple x <> Simple y = Simple (x ++ y) 

instance Monoid SimpleError where
  mempty = Simple ""

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge n) = Simple ("[index (" ++ show n ++ ") is too large]")
lie2se ErrNegativeIndex = Simple "[negative index]"

toSimple = runExcept . withExcept lie2se
xs = [1, 2, 3]

{-
GHCi> toSimple = runExcept . withExcept lie2se
GHCi> xs = [1,2,3]
GHCi> toSimple $ xs !!! 42
Left (Simple {getSimple = "[index (42) is too large]"})
GHCi> toSimple $ xs !!! (-2)
Left (Simple {getSimple = "[negative index]"})
GHCi> toSimple $ xs !!! 2
Right 3
GHCi> import Data.Foldable (msum)
GHCi> toSimpleFromList = runExcept . msum . map (withExcept lie2se)
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 42]
Left (Simple {getSimple = "[negative index][index (42) is too large]"})
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 2]
Right 3
-}

------------------------------------------------------------

newtype Validate e a = Validate { getValidate :: Either [e] a }

instance Functor (Validate e) where
  fmap = liftM

instance Applicative (Validate e) where
  pure = return
  (<*>) = ap

instance Monad (Validate e) where
  return v = Validate $ Right v
  Validate v >>= k = either (Validate . Left) k v

instance Alternative (Validate e) where
  empty = Validate $ Left []  
  Validate (Left e1) <|> Validate (Left e2) = Validate . Left $ e1 ++ e2
  Validate (Right v) <|> Validate (Left e) = Validate . Left $ e
  Validate (Left e)  <|> Validate (Right v) = Validate . Left $ e
  Validate (Right v1) <|> Validate (Right v2) = Validate . Right $ v1


collectE :: Except e a -> Validate e a
collectE exc = case (runExcept exc) of
                 (Left err) -> Validate (Left [err])
                 (Right v)  -> Validate (Right v)

validateSum :: [String] -> Validate SumError Integer
validateSum xs = let ys = tryRead <$> xs :: [Except ReadError Integer]
                     zs :: [Except SumError Integer]
                     zs = [withExcept (SumError n) exc | (n, exc) <- zip [1..] ys]
                     vs :: [Validate SumError Integer]
                     vs = collectE <$> zs
                     process sum [] = Validate $ Right sum
                     process sum (Validate v:vs) = case v of
                                                     (Left e) -> Validate v <|> (process sum vs)
                                                     (Right u) -> process (sum + u) vs
                 in
                   process 0 vs

{-
GHCi> getValidate $ validateSum ["10", "20", "30"]
Right 60
GHCi> getValidate $ validateSum ["10", "", "30", "oops"]
Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]
-}
