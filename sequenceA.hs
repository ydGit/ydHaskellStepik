seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | n > 2 = let helperPos a0 a1 a2 n =
                       if n == 2 then a2 else helperPos a1 a2 (a2+a1-2*a0) (n - 1)
                 in helperPos 1 2 3 n
       | n < 0 = let helperNeg a0 a1 a2 n =
                       if n == 0 then a0 else helperNeg ((a1 + a0 - a2) `div` 2) a0 a1 (n + 1)
                 in helperNeg 1 2 3 n
