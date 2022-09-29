--Started March 23, 2018
newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap = undefined

instance Applicative (Reader r) where
  pure = undefined
  (<*>) = undefined

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

type User = String
type Password = String
type UsersTable = [(User, Password)]

{-
local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)


Реализуйте функцию local' из прошлого задания.

Считайте, что монада Reader определена так, как на видео:

data Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r
-}

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \r -> runReader m (f r)

{-
Вспомним пример с базой пользователей и паролей:
type User = String
type Password = String
type UsersTable = [(User, Password)]

Реализуйте функцию, принимающую в качестве окружения UsersTable и возвращающую список пользователей, использующих пароль "123456" (в том же порядке, в котором они перечислены в базе).

GHCi> runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")]
["user","root"]
-}

asks :: (r -> a) -> Reader r a
asks = Reader

firstUserPwd :: Reader UsersTable Password
-- NOTE: not optimal variant, could be replace with one-liner: asks (snd . head)
firstUserPwd = do
  pwd <- asks (snd . head)
  return pwd

pwdIsBad = (==) "123456"

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
  e <- asks null
  if e then return [] else do
    (name, pwd) <- asks head
    uts <- local' tail usersWithBadPasswords
    if pwdIsBad pwd then return (name : uts) else return uts

testTable = [("user", "123456"), ("x", "hi"), ("root", "123456")]

{- Other solutions:
usersWithBadPasswords = do
  xs <- ask
  return [user | (user, password) <- xs, password == "123456"]

usersWithBadPasswords = do
  e <- ask
  return $ map fst $ filter (\x -> snd x == "123456") e

usersWithBadPasswords = do
   t <- ask
   return [fst x | x <- t, snd x == "123456"]

-- This one is beautiful
usersWithBadPasswords = asks (map fst . filter (\(_,p) -> p == "123456"))

usersWithBadPasswords = Reader $ \ups -> [u | (u,p) <- ups, p == "123456"]
-}

-- May 14 2020 Addition
((return 2) >>= (+)) >>= (*)) $ 4
-- from the right argument of >>= we see that out monad
-- m b is (a->a) we therefore the semantics of >>= is k (m e) e
-- we need to compose
-- return 2 is a monad -- a function
-- from monadic implementation it is \_ -> 2
-- so we first compose (\_ -> 2) . (+)
-- (>>=) :: (a -> b) -> (e -> a) -> (e -> b)
-- f >>= g = \e -> f . g
-- (+) is a function from a value to another function
-- (+) :: a -> a -> a that is a function from a value to monad
--  m       >>= k
-- const 2 >>= (+) = \e -> (+) (const 2 e) e
-- which is \e -> (+) 2 e = (+2)
-- Next
-- m    >>= k
-- (+2) >>= (*) = \e -> k (m e) e
-- RHS is
-- \e -> (*) (+2 e) e
-- when applied to 4 it results in
-- (*) (+2 4) 4 = (*) 6 4 = 24
-- Conclusion: seems a bit convoluted
