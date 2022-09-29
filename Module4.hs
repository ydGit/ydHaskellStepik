import Data.Char(isDigit, isSpace)
import Data.Time.Clock
import Data.Time.Format
-- import System.Locale

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

{-
Определите частичную (определенную на значениях от '0' до '9') функцию charToInt.
GHCi> charToInt '0'
0
GHCi> charToInt '9'
9
-}
charToInt :: Char -> Int
charToInt c = case c of
                '0' -> 0
                '1' -> 1
                '2' -> 2
                '3' -> 3
                '4' -> 4
                '5' -> 5
                '6' -> 6
                '7' -> 7
                '8' -> 8
                '9' -> 9

{-
Alternative solution
charToInt c = (fromEnum c) - fromEnum '0'
-}

emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'



--Тип LogLevel описывает различные уровни логирования.

data LogLevel = Error | Warning | Info

{-
Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.

GHCi> cmp Error Warning
GT
GHCI> cmp Info Warning
LT
-}
cmp :: LogLevel -> LogLevel -> Ordering
cmp Warning Info = GT
cmp Error Warning = GT
cmp Error Info = GT
cmp Info Info = EQ
cmp Warning Warning = EQ
cmp Error Error = EQ
cmp _ _ = LT


data Result = Fail | Success
--doSomeWork :: SomeData -> (Result,Int)

{- Module 4.2
Целое число можно представить как список битов со знаком.
Реализуйте функции сложения и умножения для таких целых чисел, считая,
что младшие биты идут в начале списка, а старшие — в конце. Можно
считать, что на вход не будут подаваться числа с ведущими нулями.

Passed: 828 Correct submissions: 29% You will get: 3 points

-}
data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

powers = map (2^) [0..]

bitToIntegral :: (Integral a) => Bit -> a
bitToIntegral Zero = 0
bitToIntegral One = 1

integralToBit :: (Integral a) => a -> Bit
integralToBit 0 = Zero
integralToBit 1 = One

-- consider generalizing types to (Integral a) => a
zToInteger :: Z -> Integer
zToInteger (Z Plus xs) = sum $ zipWith (*) powers (map bitToIntegral xs)
zToInteger (Z Minus xs) = negate $ sum $ zipWith (*) powers (map bitToIntegral xs)

toDigitList :: (Integral a) => a -> [a]
toDigitList 0 = []
toDigitList n = [p] ++ toDigitList q where
  p = n `mod` 2
  q = n `div` 2

toZ :: (Integral a) => a -> Z
toZ n
  | n >= 0 = Z Plus (map integralToBit (toDigitList n))
  | otherwise = Z Minus (map integralToBit (toDigitList (negate n)))

add :: Z -> Z -> Z
add z1 z2 = toZ $ zToInteger z1 + zToInteger z2

mul :: Z -> Z -> Z
mul z1 z2 = toZ $ zToInteger z1 * zToInteger z2

{- Forum Solutions
All are based similar ideas
-}

-- foo :: Bool -> Int
-- foo ~True = 1
-- foo False = 0


{- Module 4.3
Определите тип записи, который хранит элементы лога. Имя конструктора
должно совпадать с именем типа, и запись должна содержать три поля:

timestamp — время, когда произошло событие (типа UTCTime);
logLevel — уровень события (типа LogLevel);
message — сообщение об ошибке (типа String).

Определите функцию logLevelToString, возвращающую текстуальное
представление типа LogLevel, и функцию logEntryToString, возвращающую
текстуальное представление записи в виде:

<время>: <уровень>: <сообщение>

Для преобразование типа UTCTime в строку используйте функцию timeToString.
-}


timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogEntry = LogEntry {timestamp :: UTCTime,
                          logLevel :: LogLevel,
                          message :: String}

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString x = t ++ ": " ++ l ++ ": " ++ m where
  t = timeToString $ timestamp x
  l = logLevelToString $ logLevel x
  m = message x


data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = lastName $ p1}


data Shape = Circle Double | Rectangle Double Double
isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False


{-
Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой,
то есть, если имя было "Ivan", то после применения этой функции оно превратится в "I.".
Однако, если имя было короче двух символов, то оно не меняется.
-}
abbrFirstName :: Person -> Person
abbrFirstName p@(Person {firstName=(x:y:zs)}) = p{firstName = [x, '.']}
abbrFirstName p = p


data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1-x2) + abs (y1-y2)

{- Module 4.4
Плоскость разбита на квадратные ячейки. Стороны ячеек параллельны осям координат. Координаты углов ячейки с
координатой (0,0) имеют неотрицательные координаты. Один из углов этой ячейки имеет
координату (0,0). С ростом координат ячеек увеличиваются координаты точек внутри этих ячеек.

Реализуйте функции getCenter, которая принимает координату ячейки и возвращает координату ее центра, и
функцию getCell, которая принимает координату точки и возвращает номер ячейки в которой находится
данная точка. В качестве первого аргумента обе эти функции принимают ширину ячейки.
-}

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord i j) = Coord x y where
  x = w * (fromIntegral i) + w / 2
  y = w * (fromIntegral j) + w / 2

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = Coord i j where
  i = round $ (x - w/2) / w
  j = round $ (y - w/2) / w


 {- Module 4.4
Реализуйте функцию, которая ищет в строке первое вхождение символа,
который является цифрой, и возвращает Nothing, если в строке нет цифр.
-}

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs)
  | isDigit x = Just x
  | otherwise = findDigit xs


{- Реализуйте функцию findDigitOrX, использующую функцию findDigit
(последнюю реализовывать не нужно). findDigitOrX должна находить цифру в строке,
а если в строке цифр нет, то она должна возвращать символ 'X'.
Используйте конструкцию case.
-}
findDigitOrX :: [Char] -> Char
findDigitOrX x = case findDigit x of
                   Nothing -> 'X'
                   Just d -> d


{-
Maybe можно рассматривать как простой контейнер, например, как список
длины 0 или 1. Реализовать функции maybeToList и listToMaybe, преобразующие
Maybe a в [a] и наоборот (вторая функция отбрасывает все элементы списка,
кроме первого).
-}
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x


{-
Реализуйте функцию parsePerson, которая разбирает строки вида
firstName = John\nlastName = Connor\nage = 30 и возвращает либо
результат типа Person, либо ошибку типа Error.

Строка, которая подается на вход, должна разбивать по символу '\n' на
список строк, каждая из которых имеет вид X = Y. Если входная строка
не имеет указанный вид, то функция должна возвращать ParsingError.
Если указаны не все поля, то возвращается IncompleteDataError.
Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
Если в строке присутствуют лишние поля, то они игнорируются.
-}

testString1 = "firstName = John\nlastName = Connor\nage = 30\noccupation = Student"
testString2 = "firstName = John\nlastName = Connor"
testString3 = "firstName = John\nlastName = Connor\nage = 05"
testString4 = "firstName = John\nlastName = Connor\nage = 30\noccupation == Student"
testString5 = "firstName = John\nfirstName = John\nfirstName = John"
testString6 = "wrongEntry = wrongValue\n"
testString7 = "\n\n\n\n"
testString8 = "firstName = John\nlastName = Connor\nage = 30"
testString9 = "firstName = John Alexander\nlastName = Connor\nage = 30"
testString10 = "firstName = John\nlastName = Connor\nage = 30"
testString11 = "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
testString12 = "firstName=Barbarian\nlastName=Conn On\nage=30"

testList1 = [["firstName","=","John"],["lastName","=","Connor"],["age","=","05"]]
testList2 = [["firstName","=","John"],["lastName","=","Connor"]]
testList3 = [["firstName","=","John"],["lastName","=","Connor"],["age","=","25"]]

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

stripSpaces :: String -> String
stripSpaces = reverse.(dropWhile isSpace).reverse.(dropWhile isSpace)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

breakString :: String -> [[String]]
breakString = map (map stripSpaces).map (splitOn (== '=')).lines

-- Parse input string and return properly formatted "X = Y" result or error
parseString :: String -> Either Error [[String]]
parseString "" = Left ParsingError
parseString s = let ss = breakString s
                    wellFormatted [_, _] = True
                    wellFormatted _ = False
                in
                  case ss of
                    [[]] -> Left ParsingError
                    otherwise -> if (all wellFormatted ss) then Right ss else Left ParsingError

isProperAge :: String -> Bool
isProperAge "" = False
isProperAge s@(x:xs) = (all isDigit s) && (n >= 0) where
  n = (read $ [x]) :: Int

getField :: String -> [[String]] -> Maybe String
getField _ [[]] = Nothing
getField s [[x, y]]
  | s == x = Just y
  | otherwise = Nothing
getField s ([x, y]:zs)
  | s == x = Just y
  | otherwise = getField s zs

getAge :: [[String]] -> Either Error Int
getAge ss = case getField "age" ss of
  Just s -> if isProperAge s then Right ((read $ s) :: Int) else Left (IncorrectDataError s)
  Nothing -> Left ParsingError

getFirstName :: [[String]] -> Either Error String
getFirstName ss = case getField "firstName" ss of
  Just s -> Right s
  Nothing -> Left ParsingError

getLastName :: [[String]] -> Either Error String
getLastName ss = case getField "lastName" ss of
  Just s -> Right s
  Nothing -> Left ParsingError


-- Check if the data is complete.
checkData :: [[String]] -> Either Error [[String]]
checkData [[]] = Left IncompleteDataError
checkData ss = let x `isIn` ((y:_):ys) = (x == y) || x `isIn` ys
                   x `isIn` _ = False
                   y = ("firstName" `isIn` ss && "lastName" `isIn` ss && "age" `isIn` ss)
               in case y of
                 True -> Right ss
                 False -> Left IncompleteDataError

-- Extract the values for all fields
getValues :: [[String]] -> Either Error (String, String, Int)
getValues [[]] = Left IncompleteDataError
getValues ss = let fn = getFirstName ss
                   ln = getLastName ss
                   a = getAge ss
               in
                 case a of
                   Left e -> Left e
                   Right n -> case fn of
                                Left e -> Left e
                                Right f -> case ln of
                                             Left e -> Left e
                                             Right l -> Right (f, l, n)

parsePerson :: String -> Either Error Person
parsePerson s = let x = case parseString s of
                          Left e -> Left e
                          Right ss -> checkData ss
                    y = case x of
                          Left e -> Left e
                          Right ss -> getValues ss
                    z = case y of
                          Left e -> Left e
                          Right (fn, ln, a) -> Right Person {firstName=fn, lastName=ln, age=a}
                in z

{-
Compare with the staff's solution

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson s = makePerson (lineWith "firstName ") (lineWith "lastName ") (lineWith "age ")
  where
    info :: [(String, String)]
    info = map (break (== '=')) . lines $ s

    lineWith :: String -> Maybe String
    lineWith = flip lookup info

    makePerson :: Maybe String -> Maybe String -> Maybe String -> Either Error Person
    makePerson (Just firstNameA) (Just lastNameA) (Just ageA) =
      case (firstNameA, lastNameA, ageA) of
        ('=' : ' ' : firstName, '=' : ' ' : lastName, '=' : ' ' : age) ->
          case reads age of
            [(i, "")] -> Right $ Person firstName lastName i
            _         -> Left $ IncorrectDataError age
        _ -> Left ParsingError
    makePerson _ _ _ = Left IncompleteDataError

-}
