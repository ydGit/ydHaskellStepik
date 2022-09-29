--Started: March 23, 2018
module Stepik5p7 where
import Data.Monoid

newtype Writer w a = Writer {runWriter :: (a, w)} deriving Show

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter = snd . runWriter

instance Functor (Writer w) where
  fmap = undefined

instance Applicative (Writer w) where
  pure = undefined
  (<*>) = undefined

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
    in Writer (y, u `mappend` v)

-- evalWriter :: Writer w a -> w
-- evalWriter = fst . runWriter


{-
Давайте разработаем программное обеспечение для кассовых аппаратов
одного исландского магазина. Заказчик собирается описывать товары,
купленные покупателем, с помощью типа Shopping следующим образом:
-}
type Shopping = Writer ([String], (Sum Integer)) ()

purchase :: String -> Integer -> Shopping
purchase item cost = Writer ((), ([item], (Sum cost)))

total :: Shopping -> Integer
total = getSum . snd . snd . runWriter

-- items = fst . runWriter
items = fst . snd . runWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

{-
Последовательность приобретенных товаров записывается с помощью
do-нотации. Для этого используется функция purchase, которую вам
предстоит реализовать. Эта функция принимает наименование товара,
а также его стоимость в исландских кронах (исландскую крону не
принято делить на меньшие единицы, потому используется
целочисленный тип Integer). Кроме того, вы должны реализовать
функцию total:

GHCi> total shopping1
19708
-}
