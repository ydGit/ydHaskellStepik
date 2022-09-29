-- Started: March 17
module StepikM5p4 where
import Data.Char
{-
Рассмотрим язык арифметических выражений, которые состоят из чисел,
скобок, операций сложения и вычитания. Конструкции данного языка
можно представить следующим типом данных:
-}
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)
{-
Реализуйте лексер арифметических выражений. Для начала реализуйте следующую функцию:
-}
asToken :: String -> Maybe Token
asToken [] = Nothing
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken s
  | all isDigit s  = Just (Number $ read s)
  | otherwise = Nothing

{-
Она проверяет, является ли переданная строка числом (используйте
функцию isDigit из модуля Data.Char), знаком "+" или "-", открывающейся
или закрывающейся скобкой. Если является, то она возвращает нужное
значение обёрнутое в Just, в противном случае - Nothing:

GHCi> asToken "123"
Just (Number 123)

GHCi> asToken "abc"
Nothing

Далее, реализуйте функцию tokenize:
-}
-- asToken :: String -> Maybe Token
f m' s' = do
  ts <- m'
  t <- asToken s'
  return $ ts ++ [t]
tokenize :: String -> Maybe [Token]
tokenize [] = return []
tokenize s = foldl f (return []) $ words s

{-
Функция принимает на вход строку и если каждое слово является
корректным токеном, то она возвращает список этих токенов,
завёрнутый в Just. В противном случае возвращается Nothing.

Функция должна разбивать входную строку на отдельные слова по
пробелам (используйте библиотечную функцию words). Далее,
полученный список строк должен быть свёрнут с использованием
функции asToken и свойств монады Maybe:

GHCi> tokenize "1 + 2"
Just [Number 1,Plus,Number 2]

GHCi> tokenize "1 + ( 7 - 2 )"
Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]

GHCi> tokenize "1 + abc"
Nothing

Обратите внимание, что скобки отделяются пробелами от
остальных выражений!
-}
testString1 = "1 + 2"
testString2 = "1 + ( 7 - 2 )"
testString3 = "1 + abc"

{-
Пусть имеется тип данных, который описывает конфигурацию
шахматной доски:
-}
data Board = Board
{-
Кроме того, пусть задана функция
-}
nextPositions :: Board -> [Board]
nextPositions = undefined
{-
которая получает на вход некоторую конфигурацию доски и
возвращает все возможные конфигурации, которые могут получиться,
если какая-либо фигура сделает один ход. Напишите функцию:
-}
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
  | n < 0 = []
  | n == 0 = filter pred $ [b]
  | otherwise = do
      x <- (nextPositions b)
      y <- filter pred $ nextPositionsN x (n-1) pred
      return y
{-
которая принимает конфигурацию доски, число ходов n,
предикат и возвращает все возможные конфигурации досок, которые могут
получиться, если фигуры сделают n ходов и которые удовлетворяют
заданному предикату. При n < 0 функция возвращает пустой список.
-}


{-
Используя монаду списка и do-нотацию, реализуйте функцию
-}
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple n
  | n <= 0 = []
  | otherwise = do
      a <- [1..n]
      b <- [1..n]
      c <- [1..n]
      True <- return $ (a < b) && (a^2 + b^2 == c^2)
      return (a, b, c)

{-
которая принимает на вход некоторое число x
 и возвращает список троек (a,b,c)
, таких что

a^2+b^2=c^2, a>0, b>0, c>0, c≤x, a<b


Число x может быть ≤0, на таком входе должен возвращаться пустой список.

GHCi> pythagoreanTriple 5
[(3,4,5)]

GHCi> pythagoreanTriple 0
[]

GHCi> pythagoreanTriple 10
[(3,4,5),(6,8,10)]
-}
