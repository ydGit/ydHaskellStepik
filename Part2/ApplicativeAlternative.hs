We will denote 
N = Nothing
J x = Just x

===================== Part 1 ======================
Part 1: (u <|> v) <*> w = u <*> w <|> v <*> w
             LHS        =          RHS

We will need these rules

instance Functor Maybe
    f <$> N = N                (A1)
    f <$> J x = J (f x)        (A2)

instance Applicative Maybe
    (J f) <*> m = f <$> m      (B1)
    N     <*> m = N            (B2)

instance Alternative Maybe
    N     <|> m = m            (C1)
    (J x) <|> m = (J x)        (C2)



Need to check 8 cases:
   u        v      w
1. N        N      N
2. N        N      J x
3. N        J x    N
4. N        J x    J y
5. J x      N      N
6. J x      N      J y
7. J x      J y    N
8. J x      J y    J z


1. u = v = w = N

LHS = (N <|> N) <*> N -- (C1)
              N <*> N -- (B2)
                N

RHS = N <*> N <|> N <*> N -- (B2)
            N <|> N       -- (C1)
            N

LHS = RHS is true

2. u = v = N, w= J x

LHS = (N <|> N) <*> J x  -- (C1)
              N <*> J x  -- (B2)
                N

RHS = N <*> J x <|> N <*> J x -- (B2)
              N <|> N         -- (C1)
                N

LHS = RHS is true

3. u = N, v = J x, w = N

LHS = (N <|> J x) <*> N  -- (C1)
              J x <*> N  -- (B1)
                x <$> N -- (A1)
                   N

RHS = N <*> N <|> J x <*> N  -- (B2)
            N <|> J x <*> N  -- (B1+A1)
            N <|> N          -- (C1)
                N

LHS = RHS is true

4. u = N, v = J x, w = J y

LHS = (N <|> J x) <*> J y   -- (C1)
              J x <*> J y   -- (B1)
                x <$> J y   -- (A2)
                J (x y)

RHS = N <*> J y <|> J x <*> J y -- (B2)
              N <|> J x <*> J y -- (B1+A2)
              N <|> J (x y)     -- (C1)
                J (x y)

LHS = RHS is true.

5. u = N, v = N, w = N

LHS = (J x <|> N) <*> N -- (C2)
              J x <*> N -- (B2)
                x <$> N -- (A1)
                    N

RHS = (J x) <*> N <|> N <*> N   -- (B1 + A1)
                N <|> N <*> N   -- (B2)
                N <|> N         -- (C1)
                 N
LHS = RHS is true.

6. u = J x, v = N, w = J y

LHS = (J x <|> N) <*> J y   -- (C2)
              J x <*> J y -- (B1+A2)
                J (x y)

RHS = J x <*> J y <|> N <*> J y -- (B1 + A2)
          J (x y) <|> N         -- (C2)
                J (x y)

LHS = RHS

7. u = J x, w = J y, w = N

LHS = (J x <|> J y) <*> N   -- (C2)
                J x <*> N   -- (B1)
                  x <$> N   -- (A1)
                    N

RHS = (J x <*> N) <|> (J y <*> N) -- (B1)
         x <$> N) <|> (y <$> N)   -- (A1)
                N <|> N           -- (C1)
                   N

LHS = RHS is true

8.u = J x, v = J y, w = J z

LHS = (J x <|> J y) <*> J z -- (C2)
                J x <*> J z -- (B1 + A2)
                  J (x z)

RHS = (J x <*> J z) <|> (J y <*> J z)   -- (B1+A2)
            J (x z) <|> J (y z)         -- (C2)
                  J (x z)

LHS = RHS

Part 1 proof completed.

====================== Part 2=====================

Part 2: (u `mplus` v) >>= w = (u >>= w) `mplus` (v >>= w)
                 LHS        =             RHS

We will need these rules

instance Monad Maybe
    (J x) >>= k = k x          (D1)
        N >>= k = N            (D2)

instance MonadPlus Maybe
    N `mplus` m = m            (E1)
(J x) `mplus` m = J x          (E2)


Need to check 4 basic cases:
   u        v      w
1. N        N      k
2. N        J x    k
3. J x      N      k
4. J x      J y    k


1. u = v = N

LHS = (N `mplus` N) >>= k   -- (E1)
                  N >>= k   -- (D2)
                    N

RHS = (N >>= k) `mplus` (N >>= k)   -- (D2)
              N `mplus` N           -- (E1)
                    N
LHS = RHS is true for all k.


2. u = N, v = J x, w = k

LHS = (N `mplus` J x) >>= k -- (E1)
                  J x >>= k -- (D1)
                    k x

RHS = (N >>= k) `mplus` (J x >>= k) -- (D2)
              N `mplus` (J x >>= k) -- (E1)
                J x >>= k           -- (D1)
                    k x

LHS = RHS is true for all k

3. u = J x, v = N x, w = k

LHS = (J x `mplus` N) >>= k -- (E2)
                  J x >>= k -- (D1)
                    k x

RHS = (J x >>= k) `mplus` (N >>= k) -- (D1)
            (k x) `mplus` (N >>= k) -- (D2)
            (k x) `mplus` N

When k x = N we have
                N `mplus` N -- (E1)
                    N
When k x = J y we have
                (J y) `mplus` N -- (E2)
                J y
Therefore
                (k x) `mplus` N = k x for all k

and 

LHS = RHS is true for all k.

4. u = J x, v = J y, w = k

LHS = (J x `mplus` J y) >>= k   -- (E2)
                    J x >>= k   -- (D1)
                        k x

RHS = (J x >>= k) `mplus` (J y >>= k)   -- (D1)
            (k x) `mplus` (k y)

When k x = N and k y = J z we have

LHS = N
RHS = N `mplus` (J z)   -- (E1)
         J z

LHS == RHS is NOT true for all k.



Part 2 completed.
