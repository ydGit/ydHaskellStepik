{-# LANGUAGE InstanceSigs #-}

module WriterT where
import Control.Applicative (liftA2)
import Data.Tuple (swap)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

--import Control.Monad.Fail

newtype Writer w a = Writer { runWriter :: (a, w) }

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT  = fmap snd . runWriterT

instance Functor (Writer w) where
    fmap f wr = Writer (f val, log)
        where (val, log) = runWriter wr

instance Functor m => Functor (WriterT w m) where
  fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
  fmap f = WriterT . fmap updater . runWriterT
    where updater ~(x, log) = (f x, log)

{- my variant
instance Functor m => Functor (WriterT w m) where
    fmap f wrt = WriterT mp
        where mp = swap <$> (f<$>) <$> swap <$> runWriterT wrt
-}

-------------------- Applicative

instance Monoid w => Applicative (Writer w) where
  pure v = Writer $ (v, mempty)
  awf <*> awv = Writer $ (v', log') where
    v' = (fst . runWriter $ awf) (fst . runWriter $ awv)
    log' = (snd . runWriter $ awf) `mappend` (snd . runWriter $ awv)
-- perhaps should have used pattern matching after runWriter

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure v = WriterT $ pure (v, mempty)
  awtf <*> awtv = WriterT $ liftA2 (,) v' log'
    where
      v' = (fst <$> runWriterT awtf) <*> (fst <$> runWriterT awtv)
      log' = liftA2 mappend (snd <$> runWriterT awtf) (snd <$> runWriterT awtv)

wTest1 = runWriter (Writer ((*6), "Hello ") <*> Writer (7, "world!"))

wtTest1 :: [(Int, String)]
wtTest1 = runWriterT (writer ((*6), "Hello ") <*> writer (7, "world!"))

------------------------------------------------------------
newtype StrictWriter w a = StrictWriter { runStrictWriter :: (a, w) }

instance Functor (StrictWriter w) where
  fmap f  = StrictWriter . updater . runStrictWriter
    where updater (x, log) = (f x, log)

instance Monoid w => Applicative (StrictWriter w) where
  pure x  = StrictWriter (x, mempty)

  f <*> v = StrictWriter $ updater (runStrictWriter f) (runStrictWriter v)
    where updater (g, w) (x, w') = (g x, w `mappend` w')

actionLazy = Writer (42,"Hello!")
actionStrict = StrictWriter (42,"Hello!")

------------------------------------------------------------
instance Monoid w => Monad (Writer w) where
  return  = pure
  mwv >>= k = Writer (v', log'')
    where
      (v, log) = runWriter mwv
      (v', log') = runWriter $ k v
      log'' = log `mappend` log'

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return :: a -> WriterT w m a
  return x = writer (x, mempty)

  (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  m >>= k  = WriterT $ do
      ~(v, w)   <- runWriterT m
      ~(v', w') <- runWriterT (k v)
      return (v', w `mappend` w')

instance (Monoid w, MonadFail m) => MonadFail (WriterT w m) where
  -- fail :: String -> WriterT w m a
  fail = WriterT . fail

  -- mwtv >>= k = WriterT $ liftA2 (,) x mlog''
  --   where
  --     mv' = fst <$> runWriterT mwtv
  --     mlog' = snd <$> runWriterT mwtv
  --     z = mv' >>= (runWriterT . k)
  --     x = fst <$> z -- m b
  --     y = snd <$> z -- m w, second log (log'')
  --     mlog'' = liftA2 mappend mlog' y

{-
A better (and correct one) solution is the following

m >>= k = WriterT $ do
    (v, w) <- runWriterT m
    (v', w') <- runWriterT (k v)
    return (v', w `mappend` w')

-}
wTest2 = runWriter $ do {x <- Writer (41, "Hello"); return (succ x)}

wtTest2 :: [(Int, String)]
wtTest2 = runWriterT $ do {x <- writer (41, "Hello"); return (succ x)}

wtTest3 :: [(Int, String)]
wtTest3 = runWriterT $ do {x <- WriterT [(41, "Hello"), (1, "AAA")]; return (succ x)}

------------------------------------------------------------
{-


Сделайте на основе типа данных

data Logged a = Logged String a deriving (Eq,Show)

трансформер монад LoggT :: (* -> *) -> * -> * с одноименным-}
{-конструктором данных и меткой поля runLoggT:

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

Для этого реализуйте для произвольной монады m представителя класса-}
{-типов Monad для LoggT m :: * -> *:

instance Monad m => Monad (LoggT m) where
  return x = undefined
  m >>= k  = undefined
  fail msg = undefined

Для проверки используйте функции:
-}
logTst :: LoggT Identity Integer
logTst = do
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

{-
которые при правильной реализации монады должны вести себя так:

GHCi> runIdentity (runLoggT logTst)
Logged "AAABBB" 42
GHCi> runLoggT $ failTst [5,5]
[Logged "A" 42,Logged "A" 42]
GHCi> runLoggT $ failTst [5,6]
[Logged "A" 42]
GHCi> runLoggT $ failTst [7,6]
[]
-}
data Logged a = Logged String a deriving (Eq,Show)

instance Functor Logged where
  fmap f (Logged s v) = Logged s (f v)

instance Applicative Logged where
  pure v = Logged "" v
  (Logged s f) <*> (Logged s' v) = Logged (s ++ s') (f v)

instance Monad Logged where
  return = pure
  (Logged s v) >>= k = let (Logged s' v') = k v
                       in
                         Logged (s ++ s') v'


newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Functor m => Functor (LoggT m) where
  fmap f ltm = LoggT $ (fmap f) <$> (runLoggT ltm)

instance Applicative m => Applicative (LoggT m) where
  pure = LoggT . pure . pure
  alf <*> alv = LoggT $ liftA2 (<*>) (runLoggT alf) (runLoggT alv)

instance Monad m => Monad (LoggT m) where
  return = pure
  mv >>= k  = LoggT $ do
    (Logged s v) <- runLoggT mv
    (Logged s' v') <- runLoggT (k v)
    return (Logged (s ++ s') v')

instance MonadFail m => MonadFail (LoggT m) where
  fail = LoggT . fail

------------------------------------------------------------

--class MonadTrans t where
--  lift :: Monad m => m a -> t m a


instance (Monoid w) => MonadTrans (WriterT w) where
  -- runWriterT :: m (a, w) }
  -- m a -> t m a
  -- lift mv = WriterT $ liftA2 (,) mv (pure mempty)
  lift mv = WriterT $ do
    x <- mv
    return (x, mempty)

wl3 = WriterT $ [(1, "one"), (10, "ten"), (20, "twenty")]
wtTest4 = runWriterT $ do
  x <- wl3
  f <- lift [pred, succ];
  return (f x)

--- Useful interface functions for Writer/WriterT monad
tell :: Monad m => w -> WriterT w m ()
tell w = WriterT $ return ((), w)

listen :: (Monad m) => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
  ~(a, w) <- runWriterT m
  return ((a, w), w)

censor :: (Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
  ~(a, w) <- runWriterT m
  return (a, f w)

------------------------------------------------------------
{-


Напишите функцию write2log обеспечивающую трансформер LoggT стандартным логгирующим интерфейсом:

write2log :: Monad m => String -> LoggT m ()
write2log = undefined

Эта функция позволяет пользователю осуществлять запись в лог в процессе вычисления в монаде LoggT m для любой монады m. Введите для удобства упаковку для LoggT Identity и напишите функцию запускающую вычисления в этой монаде

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = undefined

Тест
-}
logTst' :: Logg Integer
logTst' = do
  write2log "AAA"
  write2log "BBB"
  return 42
{-
должен дать такой результат:

GHCi> runLogg logTst'
Logged "AAABBB" 42

А тест (подразумевающий импорт Control.Monad.Trans.State и Control.Monad.Trans.Class)
-}
stLog :: StateT Integer Logg Integer
stLog = do
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100
{-
— такой:

GHCi> runLogg $ runStateT stLog 2
Logged "30" (300,42)


-}

-- newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }
write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
-- LoggT Identity a -> Logged a
runLogg = runIdentity . runLoggT
-- runLogg = undefined

------------------------------------------------------------
{-


В последнем примере предыдущей задачи функция lift :: (MonadTrans t, Monad m) => m a -> t m a позволяла поднять вычисление из внутренней монады (в примере это был Logg) во внешний трансформер (StateT Integer). Это возможно, поскольку для трансформера StateT s реализован представитель класса типов MonadTrans из Control.Monad.Trans.Class.

Сделайте трансформер LoggT представителем этого класса MonadTrans, так-}
{-чтобы можно было поднимать вычисления из произвольной внутренней-}
{-монады в наш трансформер:

instance MonadTrans LoggT where
  lift = undefined

-}
logSt :: LoggT (State Integer) Integer
logSt = do
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100

{-
Проверка:

GHCi> runState (runLoggT logSt) 2
(Logged "30" 300,42)

-}

-- newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }
-- data Logged a = Logged String a deriving (Eq,Show)
instance MonadTrans LoggT where
  -- m a -> t m a
  -- m a -> LoggT m a
  lift mv = LoggT $ do
    v <- mv
    return (pure v)
