-- Exercises
ap :: Monad m => m (a -> b) -> m a -> m b
ap fs as = do
    f <- fs
    a <- as
    pure (f a)

pairs :: [a] -> [b] -> [(a, b)]
pairs as bs = as >>= 
    (\a -> bs >>= \b -> pure (a, b))
