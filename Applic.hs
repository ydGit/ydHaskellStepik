import Data.Char
import Control.Applicative

import Text.Parsec hiding (char, many1, many, (<|>), digit)

-- Part 2: 1.4
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f p = Prs (\s -> let res = runPrs p s in case res of
                                                  Just (x, y) -> Just (f x, y)
                                                  otherwise -> Nothing)
instance Applicative Prs where
  pure x = Prs (\ s -> Just (x, s))
  (<*>) pf pv = Prs (\ s -> let res = runPrs pf s in
                        case res of
                          Nothing -> Nothing
                          Just (f, s') -> let res' = runPrs pv s' in case res' of
                                                                       Nothing -> Nothing
                                                                       Just (x, s'') -> Just (f x, s'')
                    )
instance Alternative Prs where
  empty = Prs (\ s -> Nothing)
  pl <|> pr = Prs (\s -> let res = runPrs pl s in
                      case res of
                        Nothing -> runPrs pr s
                        otherwise -> res)

char :: Char -> Prs Char
char c = Prs (\ s -> case s of
                 [] -> Nothing
                 (x:xs) -> if (x == c) then Just (x, xs) else Nothing
             )

--Like many but must be successfull at least once
many1 :: Prs a -> Prs [a]
many1 p = (:) <$> (p <|> empty) <*> many p

digit :: Prs Char
digit = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

nat :: Prs Int
nat = Prs (\ s -> let res = runPrs (many1 digit) s in case res of
                                                          Nothing -> Nothing
                                                          Just (x, xs) -> Just (read x :: Int, xs)
          )

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat
{- Expected behavior
GHCi> runPrs mult "14*3"
Just (42,"")
GHCi> runPrs mult "64*32"
Just (2048,"")
GHCi> runPrs mult "77*0"
Just (0,"")
GHCi> runPrs mult "2*77AAA"
Just (154,"AAA")
-}
