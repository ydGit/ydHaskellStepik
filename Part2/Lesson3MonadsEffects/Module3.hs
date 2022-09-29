module Module3 where

import Control.Monad (liftM, ap, MonadPlus(mzero, mplus), guard, msum)
import Control.Applicative (Alternative(empty, (<|>)))

newtype Except e a = Except {runExcept :: Either e a}

except :: Either e a -> Except e a
except  = Except

instance Functor (Except e) where
  fmap = liftM


instance Applicative (Except e) where
  pure = return
  (<*>) = ap

instance Monad (Except e) where
  return a = except . Right $ a
  m >>= k = case runExcept m of
              (Left e) -> except . Left $ e
              (Right v) -> k v

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f exc = case runExcept exc of
                     (Left e) -> except . Left $ f e
                     (Right v) -> except . Right $ v


throwE :: e -> Except e a
throwE = except . Left


catchE :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` h = case runExcept m of
                 (Right v) -> except . Right $ v
                 (Left e) -> h e

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)
