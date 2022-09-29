module MonadTrans where

import Control.Monad
import Control.Monad.Trans

import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)
import Data.Char (toUpper)

secondElem :: Reader [String] String
secondElem = do
  el2 <- asks (map toUpper . head . tail)
  return el2

logFirst :: [String] -> Writer String String
logFirst xs = do
  let el1 = head xs
  let el2 = (map toUpper . head . tail) xs
  tell el1
  return el2


logFirstAndRetSecond :: ReaderT [String] (Writer String) String
logFirstAndRetSecond = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  lift $ tell el1
  return el2


strings = ["abc", "defg", "hij"]
test = runWriter (runReaderT logFirstAndRetSecond strings)

-- Reader :: r -> a
logFirstAndRetSecond' :: WriterT String (Reader [String]) String
logFirstAndRetSecond' = do
  el1 <- lift $ asks head
  el2 <- lift $ asks (map toUpper . head . tail)
  tell el1
  return el2

test2 = runReader (runWriterT logFirstAndRetSecond') strings

{-
Эта функция принимает два предиката и список и записывает в один лог-}
{-элементы списка, удовлетворяющие первому предикату, в другой лог —-}
{-второму предикату, а возвращающает список элементов, ни одному из-}
{-них не удовлетворяющих.

GHCi> (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
(([3,4,5,6,7],[0,1,2]),[8,9,10])
-}
separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p q xs = do
  let ps = filter p xs
  let qs = filter q xs
  let ys = filter (\z -> ((not . p $ z) && (not . q $ z))) xs
  tell ps
  lift $ tell qs
  return ys

testSeparate = (runWriter . runWriterT) $ separate (<3) (>7) [0..10]

----------------------------------------

-- possibly monadic type * -> *
type MyRW = ReaderT [String] (Writer String)

runMyRW :: MyRW a -> [String] -> (a, String) -- results is value and
           -- string log
runMyRW rw e = runWriter (runReaderT rw e)

myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell

logFirstAndRetSecond'' :: MyRW String
logFirstAndRetSecond'' = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2

test3 = runMyRW logFirstAndRetSecond'' strings


logFirstAndRetSecond3 :: MyRWT IO String
--logFirstAndRetSecond3 = undefined

logFirstAndRetSecond3 = do
  el1 <- myAsks3 head
  myLift3 $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks3 (map toUpper . head . tail)
  myLift3 $ putStrLn $ "Second is " ++ show el2
  myTell3 el1
  return el2

myAsks3 :: Monad m => ([String] -> a) -> MyRWT m a
myAsks3 = asks

-- myTell :: String -> MyRW ()
myTell3 :: Monad m => String -> MyRWT m ()
myTell3 = lift . tell

myLift3 :: Monad m => m a -> MyRWT m a
myLift3 = lift . lift  -- to get to the core IO monad that is wrapped twice


-- First, wrap IO into writer transformer
wIO :: WriterT String IO String
wIO = undefined

-- Second, wrap IO into writer transformer and then into the reader
rwIO :: ReaderT [String] (WriterT String IO) String
rwIO = undefined

-- This leads to a generalization, note that the return type is not specified
-- this leads to a monad * -> *
type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt e = runWriterT (runReaderT rwt e)

----------------------------------------

myAsk3 :: MyRWT Maybe [String]
myAsk3 = myAsks3 id

safeLogFirstAndRetSecond :: MyRWT Maybe String
safeLogFirstAndRetSecond = do
  xs <- myAsk3
  case xs of
    (el1 : el2 : _) -> myTell3 el1 >> return (map toUpper el2)
    _ -> myLift3 Nothing

--
{-
Реализуйте безопасную функцию veryComplexComputation, записывающую в
лог через запятую первую строку четной длины и первую строку
нечетной длины, а возвращающую пару из второй строки четной и второй
строки нечетной длины, приведенных к верхнему регистру:

GHCi> runMyRWT veryComplexComputation ["abc","defg","hij"]
Nothing
GHCi> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
Just (("KL","HIJ"),"defg,abc")
-}

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- myAsk3
  let ys = filter (even . length) xs
  let zs = filter ((not .even) . length) xs
  let ws = zip ys zs
  case ws of
    ((ev1, odd1) : (ev2, odd2) : _) -> do
      myTell3 ev1
      myTell3 ","
      myTell3 odd1
      return (map toUpper ev2, map toUpper odd2)
    _ -> myLift3 Nothing


------------------------------------------------------------

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n


type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi m n = runState (runExceptT m) n
          

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go low high st = do
    m <- (lift $ do {st; get})
    if m <= low 
    then throwE "Lower bound" 
    else if m >= high 
         then throwE "Upper bound"; 
         else return ()
    
    
------------------------------------------------------------
-- (Integer, Integer) pair is an environment

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: RiiEsSiT m a -> (Integer,Integer) -> Integer -> m (Either String a, Integer)
runRiiEsSiT rest env n = runStateT (runExceptT (runReaderT rest env)) n 

-- runStateT :: StateT s m a -> s -> m (a, s)
-- runStateT :: Monad m => StateT Integer m Integer -> Integer -> m (Integer, Integer)

-- m is a monad here
-- RiiEsSiT m () = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) ()
-- mlift2 = lift . lift
-- mlift3 = lift . lift . lift

go' :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go' stt = do
    (low, high) <- ask
    n <- (lift . lift $ do {stt; get})
    if n <= low 
    then (lift . throwE) "Lower bound" 
    else if n >= high 
         then (lift . throwE) "Upper bound"; 
         else return ()
    {-
    -- m <- (lift $ do {st; get})
    let n = undefined
    let low = undefined
    let high = undefined
    if n <= low 
    then undefined --put $ throwE "Lower bound" 
    else if n >= high 
         then undefined --put $ throwE "Upper bound"; 
         else return ()
    -}


tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n
  
  