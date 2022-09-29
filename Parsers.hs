import Text.Parsec
import Data.Char
--import Control.Applicative

getList :: Parsec String u [String]
getList = (many1 digit) `sepBy` (char ';')

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces start stop middle = start *> middle <* stop

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

anyChr :: Prs Char
anyChr = Prs (\s -> case s of
                      [] -> Nothing
                      (x:xs) -> Just (x, xs))

-- 1.4 Step 8
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


satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE (\ s -> case s of
                      [] -> Left "unexpected end of input"
                      (x:xs) -> if p x then Right (x, xs) else Left ("unexpected " ++ x:[]))

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
