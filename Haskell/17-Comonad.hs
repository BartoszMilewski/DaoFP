composeWithEnv :: ((b, e) -> c) -> ((a, e) -> b) -> ((a, e) -> c)
composeWithEnv g f = \(a, e) -> g (f (a, e), e)

idWithEnv :: (a, e) -> a
idWithEnv (a, e) = a

{- Exercise:

-- equivalent version of composition

composeWithEnv g f (a, e) = g (f (a, e), e)

f :: (a, e) -> b
g :: (b, e) -> c
h :: (c, e) -> d

-- co-Kleisli composition resulting in (a, e) -> d

-- Two associations give the same result:

composeWithEnv h (composeWithEnv g f) (a, e) =
h ((composeWithEnv g f) (a, e), e) =
h (g (f (a, e), e), e) =

composeWithEnv (composeWithEnv h g) f (a, e) =
(composeWithEnv h g) (f (a, e), e) =
h (g (f (a, e), e), e)

-}

class Functor w => Comonad w where
   (=<=) :: (w b -> c) -> (w a -> b) -> (w a -> c)
   extract :: w a -> a

instance Comonad ((,) e) where
  g =<= f = \ea -> g (fst ea, f ea)
  extract = snd

duplicate :: Comonad w => w a -> w (w a)
duplicate = id =<= id

extend :: Comonad w => (w a -> b) -> w a -> w b
extend k = id =<= k

-- Exercise
duplicate' :: Comonad w => w a -> w (w a)
duplicate' = extend id

extend' :: Comonad w => (w a -> b) -> w a -> w b
extend' k wa = fmap k (duplicate wa)