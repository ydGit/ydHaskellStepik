module Endo where

newtype Endo a = Endo {appEndo :: a -> a}

mkEndo :: Foldable t => t (a -> a) -> Endo a
-- mkEndo cont = Endo $ foldr (.) id cont
mkEndo = Endo . foldr (.) id

{-
GHCi> e1 = mkEndo [(+5),(*3),(^2)]
GHCi> appEndo e1 2
17
GHCi> e2 = mkEndo (42,(*3))
GHCi> appEndo e2 2
6
-}
