-- Started: March 15, 2018
module Stepik5p2 where
{-
Введём следующий тип:
-}
data Log a = Log [String] a deriving Show
{-
Реализуйте вычисление с логированием, используя Log.
Для начала определите функцию toLogger
-}
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s = \x -> Log [s] (f x)

-- helper functions for testing
add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"
{-
которая превращает обычную функцию, в функцию с логированием:
GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6

Далее, определите функцию execLoggers
-}
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = let (Log ss y) = f x
                        (Log ss' z) = g y
                    in Log (ss ++ ss') z
{-
Которая принимает некоторый элемент и две функции с логированием.
execLoggers возвращает результат последовательного применения функций
к элементу и список сообщений, которые были выданы при применении
каждой из функций:

GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8
-}

{-
Реализуйте функцию returnLog
которая является аналогом функции return для контекста Log.
Данная функция должна возвращать переданное ей значение с
пустым контекстом.
-}
returnLog :: a -> Log a
returnLog x = Log [] x

{-Реализуйте фукцию bindLog, которая работает подобно оператору >>= для контекста Log.
GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1

GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8
-}
-- Reminder
-- data Log a = Log [String] a deriving Show
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log ss x) f = let (Log ss' y) = f x
                           in Log (ss ++ ss') y

instance Functor Log where
  fmap = undefined

instance Applicative Log where
  pure = undefined
  (<*>) = undefined

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

{-
Используя return и >>=, определите функцию execLoggersList,
которая принимает некоторый элемент, список функций с логированием
и возвращает результат последовательного применения всех функций
в списке к переданному элементу вместе со списком сообщений,
которые возвращались данными функциями:

GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800
-}
-- Reminder bindLog :: Log a -> (a -> Log b) -> Log b
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x [] = return x
execLoggersList x (f:fs) = let (Log ss y) = f x
                               (Log ss' z) = execLoggersList y fs
                           in
                             Log (ss ++ ss') z
