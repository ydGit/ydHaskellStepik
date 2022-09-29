--Started: March 6, 2018
import Data.Monoid

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap = undefined

instance Applicative (Reader r) where
  pure = undefined
  (<*>) = undefined

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r


newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap = undefined

instance Applicative (State s) where
  pure = undefined
  (<*>) = undefined

instance Monad (State s) where
  return x = State $ \st -> (x, st)
  m >>= k  = State $ \st ->
    let (a, st') = runState m st
        m' = k a
    in runState m' st'

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put st = State $ \_ -> ((), st)

{-
Давайте убедимся, что с помощью монады State можно
эмулировать монаду Reader. Напишите функцию readerToState,
«поднимающую» вычисление из монады Reader в монаду State:

GHCi> evalState (readerToState $ asks (+2)) 4
6
GHCi> runState (readerToState $ asks (+2)) 4
(6,4)
-}


readerToState :: Reader r a -> State r a
readerToState m = let f = runReader m
                      g = \st -> (f st, st)
                  in
                    State g

{- Other solutions:
readerToState m = State $ \st -> (runReader m st, st)
readerToState (Reader f) = State $ \x -> (f x, x)
readerToState m = state (\s -> ((runReader m) s, s))

readerToState m = do
   st <- get
   return $ runReader m st
-}

fibStep :: State (Integer, Integer) ()
fibStep = do
  (n, m) <- get
  put (m, (n + m))


execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (sequence_ $ replicate n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

{-
Некоторое время назад мы определили тип _двоичных_ деревьев,
содержащих значения в узлах:

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

В этой задаче вам дано значение типа Tree (), иными словами,
вам задана форма дерева. Требуется пронумеровать вершины дерева
данной формы, обойдя их in-order (то есть, сначала обходим
левое поддерево, затем текущую вершину, затем правое поддерево):

GHCi> numberTree (Leaf ())
Leaf 1
GHCi> numberTree (Fork (Leaf ()) () (Leaf ()))
Fork (Leaf 1) 2 (Leaf 3)
-}

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberTreeStep :: State (Sum Int, Tree Int) ()
numberTreeStep = undefined


-- Idea: keep a "state" -- running index of the enumeration
-- As we evaluate, the index is changing as well as the tree that
-- we build
numberTreeIdx :: Tree () -> Integer -> (Tree Integer, Integer)
numberTreeIdx (Leaf ()) n = (Leaf n, n+1)
numberTreeIdx (Fork lt x rt) n = let (lt', i) = numberTreeIdx lt n
                                     (rt', j) = numberTreeIdx rt (i+1)
                                 in
                                   (Fork lt' i rt', j)

numberTree :: Tree () -> Tree Integer
numberTree t = fst $ numberTreeIdx t 1

{-
Monadic solutions:
numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (number tree) 1
  where
    number :: Tree () -> State Integer (Tree Integer)
    number (Leaf ()) = get >>= \i -> modify (+1) >> return (Leaf i)
    number (Fork l () r) = do
      la <- number l
      i <- get
      modify (+1)
      ra <- number r
      return $ Fork la i ra

-}
-- This one is really good.
tick :: State Integer Integer
tick = do
    n <- get
    put (n + 1)
    return n


tickTock = do
    tick
    tick
    tick
    tick
{-
numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (numberTree' tree) 1

numberTree' (Leaf _) = tick >>= return . Leaf
numberTree' (Fork l _ r) = do
    l' <- numberTree' l
    t  <- tick
    r' <- numberTree' r
    return $ Fork l' t r'
--
numberTree' (Leaf _) = do
    n <- get
    modify((+1))
    return (Leaf n)

numberTree' (Fork a _ b) = do
    l <- numberTree' a
    n <- get
    modify((+1))
    r <- numberTree' b
    return (Fork l n r)

numberTree  tree = fst (runState (numberTree' tree) 1)
-}
