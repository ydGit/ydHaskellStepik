{-# LANGUAGE InstanceSigs #-}
module StateT where
import MonadTrans
import ReaderT hiding (MonadTrans, lift)
import Control.Applicative (liftA2)
import WriterT
import Data.Monoid

newtype State s a = State { runState :: s -> (a, s) }

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

sm :: StateT Integer Maybe Integer
sm = StateT $ \st -> Just (st + 3, st * 2)

sl3 = StateT $ \st -> [(st+1, 42), (st+2, st), (st+3, st*2)]

------------------------------------------------------------
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT stm = (fst <$>) . (runStateT stm)

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT stm = (snd <$>) . (runStateT stm)

------------------------------------------------------------
{-
Нетрудно понять, что монада State более «сильна», чем монада Reader: вторая тоже, в некотором смысле, предоставляет доступ к глобальному состоянию, но только, в отличие от первой, не позволяет его менять. Покажите, как с помощью StateT можно эмулировать ReaderT:

GHCi> evalStateT (readerToStateT $ asks (+2)) 4
6
GHCi> runStateT  (readerToStateT $ asks (+2)) 4
(6,4)
-}

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT rmt = StateT $ \r -> liftA2 (,) (runReaderT rmt r)(pure r)





{-
instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \ s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'


instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \st -> do
    a <- m
    return (a, st)


get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \_ -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)

class MonadTrans t where
  lift :: Monad m => m a -> t m a

-}
------------------------------------------------------------
-- newtype State s a = State { runState :: s -> (a, s) }
-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
instance Functor (State s) where
  fmap f st = State $ \s -> update f (runState st s) where
    update g ~(x, y) = (g x, y)

instance Functor m => Functor (StateT s m) where
  fmap f stm = StateT $ \s -> (update f) <$> (runStateT stm s) where
    update g ~(x, y) = (g x, y)

------------------------------------------------------------
instance Applicative (State s) where
  pure v = State $ \s -> (v, s)
  sf <*> sv = State $ \s -> let (f, s') = runState sf s
                                (v, s'') = runState sv s'
                            in
                              (f v, s'')
-- Q: Why do we first obtain f and not v. The order of s' and s''
-- would be reversed in this approach?

sf :: State Int (Int -> Int)
sf = State $ \s -> ((+1), s + 2)

sv :: State Int Int
sv = State $ \s -> (2 * s, s+1)

-- the following implementation is most likely wrong.
-- it does not propagate the 'evolving' state from one
-- runStateT to another

{-
instance Applicative m => Applicative (StateT s m) where
  pure v = StateT $ \s -> pure (v, s)
  sf <*> sv = StateT $ \s -> let mf = runStateT sf s -- m ((a -> b), s)
                                 mv = runStateT sv s -- m (a, s)
                                 update (f, x) (v, y) = (f v, y)
                             in
                               pure update <*> mf <*> mv
-}
-- To fix this in a simple way we must use Monad context for m
-- it allows a simple way to extract values from the context
-- this is NOT a restriction, since m is a monad to begin with!
instance Monad m => Applicative (StateT s m) where
  pure v = StateT $ \s -> pure (v, s)
  sf <*> sv = StateT $ \s -> do
    ~(f, s') <- runStateT sf s
    ~(v, s'') <- runStateT sv s'
    return (f v, s'')

{-
Check if these are monads

[] -- yes, list is a monad

StateT Int (Reader ()) -- yes, (Reader ()) is a monad, and thus StateT Int (Reader ()) is

Writer (Sum Int) -- yes, (Writer (Sum Int)) is a monad, becase (Sum Int) is a monad

StateT Int ZipList - no, StateT Int ZipList is not a monad, b.c. ZipList is not a monad

ReaderT Int ZipList - no, ReaderT Int ZipList is not a monad, b.c. ZipList is not a monad

ReaderT Int (Writer String) -- yes, (Writer String) is a monad, b.c. Sting is a monoid

-}
instance Monad (State s) where
  return = pure
  msv >>= k = State $ \s -> let (v, s') = runState msv s
                            in
                              runState (k v) s'

instance Monad m => Monad (StateT s m) where
  return = pure
  -- StateT s m a -> (a -> StateT s m b) -> StateT s m b
  -- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
  mstv >>= k = StateT $ \s -> do
    ~(v, s') <- runStateT mstv s
    runStateT (k v) s' -- already return a monad, no need to place into deeper context

------------------------------------------------------------
{-


Неудачное сопоставление с образцом для реализованного на предыдущих видео-степах трансформера StateT аварийно прерывает вычисление:

GHCi> sl2 = StateT $ \st -> [(st,st),(st+1,st-1)]
GHCi> runStateT (do {6 <- sl2; return ()}) 5
*** Exception: Pattern match failure in do expression ...

Исправьте реализацию таким образом, чтобы обработка такой ситуации переадресовывалась бы внутренней монаде:

GHCi> sl2 = StateT $ \st -> [(st,st),(st+1,st-1)]
GHCi> runStateT (do {6 <- sl2; return ()}) 5
[((),4)]
GHCi> sm = StateT $ \st -> Just (st+1,st-1)
GHCi> runStateT (do {42 <- sm; return ()}) 5
Nothing

-}

instance MonadFail m => MonadFail (StateT s m) where
  -- String -> StateT s m a
  fail st = StateT $ \s -> do
    fail st

sl2 = StateT $ \st -> [(st,st),(st+1,st-1)]
sm2 = StateT $ \st -> Just (st+1,st-1)

------------------------------------------------------------
instance MonadTrans (StateT s) where
  -- m a -> t m a
  lift mv = StateT $ \s -> do
    v <- mv
    return (v, s)


get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put st = state $ \_ -> ((), st)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)

------------------------------------------------------------

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

{-
Вам дано значение типа Tree (), иными словами, вам задана форма дерева. От вас требуется сделать две вещи: во-первых, пронумеровать вершины дерева, обойдя их in-order обходом (левое поддерево, вершина, правое поддерево); во-вторых, подсчитать количество листьев в дереве.

GHCi> numberAndCount (Leaf ())
(Leaf 1,1)
GHCi> numberAndCount (Fork (Leaf ()) () (Leaf ()))
(Fork (Leaf 1) 2 (Leaf 3),2)

Конечно, можно решить две подзадачи по-отдельности, но мы сделаем это всё за один проход. Если бы вы писали решение на императивном языке, вы бы обошли дерево, поддерживая в одной переменной следующий доступный номер для очередной вершины, а в другой — количество встреченных листьев, причем само значение второй переменной, по сути, в процессе обхода не требуется. Значит, вполне естественным решением будет завести состояние для первой переменной, а количество листьев накапливать в «логе»-моноиде.

Вот так выглядит код, запускающий наше вычисление и извлекающий результат:
-}
numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go = undefined

-- Вам осталось только описать само вычисление — функцию go.
