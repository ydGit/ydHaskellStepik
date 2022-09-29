Вспомним что
map _ [] = []
map f (x:xs) = (f x) : map f xs
------------------------------------------

Доказать:
fmap f (fmap g xs) = fmap (f . g) xs

Докажем базовый случай xs = []

fmap f (fmap g []) = fmap (f.g) []     -- (1)

Преобразуем сначала левую часть равенства (1)
fmap f (fmap g []) = fmap f (map g [])  = -- из определения fmap для списков
= fmap f [] = map f [] = []

Преобразуем правую часть равенства (1)

fmap (f.g) [] = map (f.g) [] = [] -- из определения fmap для списков и реализации map

Итак, для пустого списка равенство (1) выполняется.

**Индукционная гипотеза**: Предположим для непустого списка xs выполняется
fmap f (fmap g xs) = fmap (f . g) xs   -- (2)
покажем что следует равенство
fmap f (fmap g (x:xs)) = fmap (f . g) (x:xs)  --- (3)

Рассмотрим левую часть равенства (3):
fmap f (fmap g (x:xs)) = fmap f (map g (x:xs)) = -- из определения fmap для списков
= fmap f (g x):(map g xs) = -- из реализации map
= map f (g x):(map g xs) = -- из определения fmap
= (f (g x)):(map f (map g xs)) = -- из реализации map
= (f (g x)):(fmap f (fmap g xs)) = -- из определения fmap
= (f (g x)):(fmap (f.g) xs) = -- из индукционной гипотезы
= (f.g x):(fmap f.g xs) -- из свойства композиции функций

Рассмотрим правую часть равенства (3):
fmap (f . g) (x:xs) = map (f.g) (x:xs) = -- из определения fmap для списков
= (f.g x):(map f.g xs) = -- из определения map
= (f.g x):(fmap f.g xs) -- из определения fmap для списков

Вывод: левая и правая части равенства (3) равны для списка вида
(x:xs) если такое же равенство справедливо для более короткого
списка xs. Так как равенство справедливо для списка длины 0, то
автоматически имеем то же равенство для списков длины 1, 2, 3 и т.д.
