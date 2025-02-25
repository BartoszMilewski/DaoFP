{-# language UndecidableInstances #-}
import Prelude hiding (Monad, (>>=), return)

class Functor m => Monad' m where
  (<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)
  return' :: a -> m a

instance Monad' Maybe where
  g <=< f = \a -> case f a of
                    Nothing -> Nothing
                    Just b -> g b
  return' = Just  

class Functor m => Monad'' m where
  join :: m (m a) -> m a
  return'' :: a -> m a

{-
instance (Functor m, Monad'' m) => Monad' m where
    (<=<) :: (Functor m, Monad'' m) => (b -> m c) -> (a -> m b) -> a -> m c
    g <=< f = join . fmap g . f
    return' = return''
-}

join' :: Monad' m => m (m a) -> m a
join' = id <=< id

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a 

{-
instance Monad m => Monad' m where
    g <=< f = \a -> (f a) >>= g
    return' = return
-}

bind :: Monad' m => m a -> (a -> m b) -> m b
bind ma k = (k <=< id) ma

join''  :: (Monad m) => m (m a) -> m a
join'' mma =  mma >>= id

liftM :: Monad m => (a -> b) -> (m a -> m b)
liftM f ma = ma >>= (return . f)

class Monoidal f where
  unit  :: f ()
  (>*<) :: f a -> f b -> f (a, b)

instance Monad m => Monoidal m where
    unit = return ()
    ma >*< mb = ma >>= 
         (\a -> mb >>= 
             \b -> return (a, b))

-- Applicative splat 

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap fs as = fs >>= 
    (\f -> as >>= 
        \a -> return (f a))

{- Equivalently, using the do notation
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap fs as = do 
    f <- fs
    a <- as
    return (f a)
-}


