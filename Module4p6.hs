-- Started: March 13, 2018
module Demo where
import Prelude hiding (lookup)
-- import qualified Data.List as L

newtype Identity a = Ident a deriving Show

-- newtype A a b = A b -- [v]
-- newtype A a b = A a b -- [x]
-- newtype A a b = A a -- [v]
-- newtype A = A A -- [v]
-- newtype A a = A a a -- [x]
-- newtype A a = A a -- [v]
-- newtype A = A A A -- [x]
-- newtype A a = A -- [x]
-- newtype A = A a -- [x]
-- newtype A = A -- [x]

{- Реализуйте представителя класса типов Monoid для типа Xor,
в котором mappend выполняет операцию xor.-}

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    Xor a `mappend` Xor b = Xor (a /= b)

newtype First a = First { getFirst :: Maybe a} deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
  mempty = First Nothing
  First Nothing `mappend` r = r
  l `mappend` _ = l

{-
Реализуйте представителя класса типов Monoid для Maybe' a так, чтобы mempty
не был равен Maybe' Nothing. Нельзя накладывать никаких дополнительных ограничений
на тип a, кроме указанных в условии.
-}
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    (Maybe' Nothing) `mappend` _ = Maybe' Nothing
    _ `mappend` (Maybe' Nothing) = Maybe' Nothing
    (Maybe' (Just x)) `mappend` (Maybe' (Just y)) = Maybe' (Just (x `mappend` y))

{-
Ниже приведено определение класса MapLike типов, похожих на тип Map.
Определите представителя MapLike для типа ListMap, определенного ниже как
список пар ключ-значение. Для каждого ключа должно храниться не больше одного
значения. Функция insert заменяет старое значение новым, если ключ
уже содержался в структуре.
-}

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
  empty = ListMap []


  lookup k (ListMap []) = Nothing
  lookup k lm = let ((kk, vv):kvs) = getListMap lm
                in
                  if k == kk then (Just vv) else lookup k (ListMap kvs)

  insert k v (ListMap []) = ListMap [(k, v)]
  insert k v lm = let ((kk, vv):kvs) = getListMap lm
                  in
                    if k == kk then ListMap ((k, v):kvs) else
                      let kvs' = getListMap $ insert k v (ListMap kvs)
                      in ListMap ((kk, vv):kvs')

  delete k lm@(ListMap []) = lm
  delete k lm = let ((kk, vv):kvs) = getListMap lm
                in
                  if k == kk then ListMap kvs else
                    let kvs' = getListMap $ delete k (ListMap kvs)
                        in ListMap ((kk, vv):kvs')

  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

{-
Реализуйте представителя MapLike для типа ArrowMap, определенного ниже.
-}
newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty = ArrowMap (\_ -> Nothing)
  lookup k am = getArrowMap am $ k
  insert k v am = ArrowMap f
    where f = \x -> if x == k then Just v else getArrowMap am $ x
  delete k am = ArrowMap f
    where f = \x -> if x == k then Nothing else getArrowMap am $ x

  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)
