strength' :: Functor f => (a, f b) -> f (a, b)
strength' = uncurry (\a -> fmap (coeval a))

coeval :: a -> (b -> (a, b))
coeval a = \b -> (a, b)

strength :: Functor f => (a, f b) -> f (a, b)
strength (a, bs) = fmap (a, ) bs