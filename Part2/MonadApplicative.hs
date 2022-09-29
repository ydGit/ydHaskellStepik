module MonadApplicative where

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) cz = Bi x y cz
concat3OC (Un x) (Bi y1 y2 cy) cz = Bi x y1 (concat3OC (Un y2) cy cz)
concat3OC (Bi x1 x2 cx) cy cz = Bi x1 x2 (concat3OC cx cy cz)

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')

tstResult = Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

tstPass = (concat3OC tst1 tst2 tst3) == tstResult

{-
GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concat3OC tst1 tst2 tst3
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

-}

-- Next Step
concatOC :: OddC (OddC a) -> OddC a
concatOC (Un mx) = mx
concatOC (Bi (Un x) (Un y) mz) = Bi x y (concatOC mz)
concatOC (Bi (Un x) (Bi y1 y2 my) mz) = Bi x y1 (concatOC (Bi (Un y2) my mz))
concatOC (Bi (Bi x1 x2 mx) my mz) = Bi x1 x2 z
  where z = concat3OC mx my (concatOC mz)
-- concatOC _ = undefined
{-
GHCi> concatOC $ Un (Un 42)
Un 42
GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concatOC $ Bi tst1 tst2 (Un tst3)
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
-}


-- Next Step
instance Functor OddC where
  fmap f (Un x) = Un (f x)
  fmap f (Bi x y cz) = Bi (f x) (f y) (fmap f cz)

instance Applicative OddC where
  pure = Un
  (Un f) <*> cv = f <$> cv
  (Bi f1 f2 cf) <*> cv = concat3OC (f1 <$> cv) (f2 <$> cv) (cf <*> cv)

instance Monad OddC where
  return = pure
  (Un v) >>= f = f v
  (Bi x y cz) >>= f = concat3OC (f x) (f y) (cz >>= f)

tstA1 = Bi 10 20 (Un 30)
tstA2 = Bi 1 2 (Bi 3 4 (Un 5))

{-
GHCi> do {x <- tstA1; y <- tstA2; return (x + y)}
Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))
GHCi> do {x <- tstA2; y <- tstA1; return (x + y)}
Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))
-}
