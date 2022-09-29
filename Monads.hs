module Monads where


safeHead = do
  b <- null  -- list monad
  if b
    then return Nothing -- but return Maybe
    else do
    h <- head  -- again list monad
    return $ Just h


-- Turns out I do not understand list as monad :(
playWithList :: [Int] -> [Int]
playWithList = do
  x <- id
  return $ fmap (2*) x
