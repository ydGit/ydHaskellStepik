{-# LANGUAGE InstanceSigs #-}


module ReaderT where
-- import Control.Monad.Trans.Class (lift)

newtype Reader r a = Reader { runReader :: r -> a }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T g 
    where g x y = return $ f x y

newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T g 
    where g x y z = return $ f x y z
    
    
instance Functor (Reader r) where
    fmap f rdr = Reader $ f . runReader rdr
    
instance (Functor m) => Functor (ReaderT r m) where
    fmap f rdr = ReaderT $ fmap f . runReaderT rdr
    
instance Applicative (Reader r) where
    pure = Reader . const
    f <*> v = Reader $ \env -> runReader f env (runReader v env)

instance Applicative m => Applicative (ReaderT r m) where
    pure = ReaderT . const . pure
    f <*> v = ReaderT $ \env -> runReaderT f env <*> (runReaderT v env)

instance Monad (Reader r) where
    return = pure
    -- Reader r va >>= va -> Reader r vb
    rv >>= k = Reader g 
        where g env = runReader (k $ runReader rv env) env
        

-- this was surprisingly easy, using reader semantics
instance Monad m => Monad (ReaderT r m) where
    return = pure
    -- ReaderT r m va >>= va -> ReaderT r m vb
    rmv >>= k = ReaderT g
        where g env =  (runReaderT rmv env) >>= \mv -> runReaderT (k mv) env
        -- this can be rewritten in do notation for inner monad
{-


Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

представителями класса типов Functor в предположении, что m является функтором:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
GHCi> (getArr2T $ succ <$> a2l) 10 100
[11,101,111]
GHCi> a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)
GHCi> (getArr3T $ sqrt <$> a3e) 2 3 4
Right 3.0


-}

instance (Functor m) => Functor (Arr2T e1 e2 m) where
    fmap f arr2t = Arr2T g
        where g x y = f <$> getArr2T arr2t x y
        
instance (Functor m) => Functor (Arr3T e1 e2 e3 m) where
    fmap f arr3t = Arr3T g
        where g x y z = f <$> getArr3T arr3t x y z
        

{-
Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

представителями класса типов Applicative в предположении, что m является аппликативным функтором:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> a2fl = Arr2T $ \e1 e2 -> [(e1*e2+),const 7]
GHCi> getArr2T (a2fl <*> a2l) 2 10
[22,30,7,7]
GHCi> a3fl = Arr3T $ \e1 e2 e3 -> [(e2+),(e3+)]
GHCi> a3l = Arr3T $ \e1 e2 e3 -> [e1,e2]
GHCi> getArr3T (a3fl <*> a3l) 3 5 7
[8,10,10,12]


-}

instance (Applicative m) => Applicative (Arr2T e1 e2 m) where
    pure x = Arr2T g 
        where g y z = pure x
    f <*> v = Arr2T $ \e1 e2 -> (getArr2T f e1 e2) <*> (getArr2T v e1 e2)
    
instance (Applicative m) => Applicative (Arr3T e1 e2 e3 m) where
    pure x = Arr3T g
        where g u v w = pure x
    f <*> v = Arr3T $ \e1 e2 e3 -> (getArr3T f e1 e2 e3) <*> (getArr3T v e1 e2 e3)
    
a2l = Arr2T $ \e1 e2 -> [e1,e2]
a2fl = Arr2T $ \e1 e2 -> [(e1*e2+),const 7]

a3fl = Arr3T $ \e1 e2 e3 -> [(e2+),(e3+)]
a3l = Arr3T $ \e1 e2 e3 -> [e1,e2]


rl3 = ReaderT $ \env -> [42, env, env*2]


{-


Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

представителями класса типов Monad в предположении, что m является монадой:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5
[6,8,8,10]
GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
GHCi> getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4
Just 81
-}
-- a2l = Arr2T $ \e1 e2 -> [e1,e2]
a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)

instance Monad m => Monad (Arr2T e1 e2 m) where
    return = pure
    amv >>= k = Arr2T g
        where g env1 env2 =  (getArr2T amv env1 env2) >>= \mv -> getArr2T (k mv) env1 env2
    
instance Monad m => Monad (Arr3T e1 e2 e3 m) where
    return = pure
    amv >>= k = Arr3T g
        where g env1 env2 env3 =  (getArr3T amv env1 env2 env3) >>= \mv -> getArr3T (k mv) env1 env2 env3
    -- String -> Arr3T e1 e2 e3 m a
    fail s = Arr3T g    
        where g _ _ _ = fail s

class MonadTrans t where
    lift :: Monad m => m a -> t m a
    
-- this was easy. just matched the types    
instance MonadTrans (ReaderT r) where
    lift mv = ReaderT g 
        where g env = mv
        
testLift = runReaderT rd 7
    where rd = do
            x <- rl3
            y <- lift (replicate 3 10)
            return (x+y)
            
            
ask :: Monad m => ReaderT r m r
ask = ReaderT return

asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = ReaderT $ return . f

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f rma = ReaderT $ \env -> runReaderT rma (f env)


{-
Сделайте трансформер

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }

представителями класса типов MonadTrans:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> getArr2T (do {x <- a2l; y <- lift [10,20,30]; return (x+y)}) 3 4
[13,23,33,14,24,34]

Реализуйте также «стандартный интерфейс» для этой монады — функцию

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a

работающую как asks для ReaderT, но принимающую при этом функцию от обоих наличных окружений:

GHCi> getArr2T (do {x <- asks2 const; y <- asks2 (flip const); z <- asks2 (,); return (x,y,z)}) 'A' 'B'
('A','B',('A','B'))
-}

instance MonadTrans (Arr2T e1  e2) where
    lift mv = Arr2T g
        where g _ _ = mv
        
asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T g
    where g env1 env2 = return $ f env1 env2