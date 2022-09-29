module Main where

import System.IO

coins :: (Num a) => [a]
coins = [2, 3, 7]

mychange :: (Ord a, Num a) => [a] -> a -> [[a]]
mychange cns@(c:cs) s
  | s < c = []
  | otherwise = [(x:xs) | x <- cns, xs <- (let ys = mychange cns (s - x) in if ys /= [] then ys else [[]]), sum (x:xs) == s]

change :: (Ord a, Num a) => a -> [[a]]
change = mychange coins

main :: IO ()
main = do
  putStr "Enter sum to split: "
  hFlush stdout
  s <- getLine
  putStrLn $ "Here are ways to split it into " ++ show coins ++ "\n" ++ show (change $ read s)
