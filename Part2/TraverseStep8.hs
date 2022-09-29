{--
sequenceA :: Applicative f => t (f a) -> f (t a)
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

Note: traverse takes a function from a to some (f b) and maps it into
a function in the foldable space; that is we have a function
(t a) -> f (t b)
instead of
a -> f b

as if we replaced a -> t a ; b -> t b


Exercise: implement sequenceA using traverse.
x :: t (f a)
sequenceA = traverse id

this is clear from the type of traverse and sequence
traverse :: (a -> f b) -> t a -> f (t b)
consider the case when a = f b, then
traverse' :: (f b -> f b) -> t (f b) -> f (t b) or equivalently
traverse' :: (f a -> f a) -> t (f a) -> f (t a)

the natural and trivial function (f a -> f a) is id. Thus

traverse . id :: t (f a) -> f (t a) -- a good candidate for sequenceA.


How about the other way?

Say we have sequenceA :: t (f a) -> f (t a) and want to build a function

myTraverse :: (a -> f b) -> t a -> f (t b)
we have not only sequenceA, but also a function
g :: (a -> f b)
and an argument
x :: t a
myTraverse g x
other constraints: f is an Applicative functor, t is a foldable context.
myTraverse = sequenceA . fmap


Moral: Traversable is implemented in a way similar to fmap
--}
