module MonadParser where

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
  fmap f pe = PrsE (\ s -> let res = runPrsE pe s in case res of
                                                       Left xs -> Left xs
                                                       Right (x, xs) -> Right (f x, xs))
instance Applicative PrsE where
  pure x = PrsE (\ s -> Right (x, s))
  pel <*> per = PrsE (\ s -> let res = runPrsE pel s in
                         case res of
                           Left xs -> Left xs
                           Right (x, xs) -> let res' = runPrsE per xs in
                             case res' of
                               Left ys -> Left ys
                               Right (y, ys) -> Right (x y, ys))

instance Monad PrsE where
  p >>= f = PrsE (\s -> let res = runPrsE p s in
                     case res of
                       Left xs -> Left xs
                       -- x :: a
                       Right (x, xs) -> let res' = runPrsE (f x) xs in
                         case res' of
                           Left ys -> Left ys
                           Right (y, ys) -> Right (y, ys))

{-
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC"
Right (('A','B'),"C")
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD"
Left "unexpected C"
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD"
Left "unexpected B"
-}

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE (\ s -> case s of
                      [] -> Left "unexpected end of input"
                      (x:xs) -> if p x then Right (x, xs) else Left ("unexpected " ++ x:[]))

-- Next step
ap :: Monad m => m (a -> b) -> m a -> m b
mf `ap` mv = do {f <- mf; v <- mv; return (f v)}

-- Next step
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mv = do {v <- mv; return (f v)}
