{-# language UndecidableInstances #-}
import Prelude hiding (Applicative, pure, (<*>), (<$>), (*>), (<*))

combine :: Maybe a -> Maybe b -> Maybe (a, b)
combine (Just a) (Just b) = Just (a, b)
combine _ _ = Nothing

ignoreMe :: Maybe ()
ignoreMe = Just ()

class Monoidal f where
  unit  :: f ()
  (>*<) :: f a -> f b -> f (a, b)

instance Monoidal Maybe where
  unit  = Just ()
  Just a >*< Just b = Just (a, b)
  _ >*< _ = Nothing

run3 :: (Functor f, Monoidal f) => 
  (x -> f a) -> (y -> f b) -> (z -> f c) -> 
  (x, y, z) -> f (a, b, c)
run3 f1 f2 f3 (x, y, z) =
  let fab  = f1 x >*< f2 y
      fabc = fab >*< f3 z
  in fmap reassoc fabc

reassoc :: ((a, b), c) -> (a, b, c)
reassoc ((a, b), c) = (a, b, c)

-- Applicative

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance (Functor f, Monoidal f) => Applicative f where
  pure a = fmap (const a) unit
  fs <*> as = fmap apply (fs >*< as)
  -- ff <*> fa = fmap (uncurry ($)) (ff >*< fa)

apply :: (a -> b, a) -> b
apply (f, a) = f a

{-
instance Applicative f => Monoidal f where
  unit = pure ()
  fa >*< fb = fmap (,) fa <*> fb
-}

pairWith :: Int -> (String -> (Int, String))
pairWith a = \x -> (a, x)

pairWith' :: Int -> [String -> (Int, String)]
pairWith' a = [\x -> (a, x)]

strength :: Functor f => (e, f a) -> f (e, a)
strength (e, as) = fmap (e, ) as

-- Exercises
instance Monoidal [] where
  unit :: [()]
  unit = () : unit
  (>*<) :: [a] -> [b] -> [(a, b)]
  as >*< [] = []
  [] >*< bs = []
  (a : as) >*< (b : bs) = (a, b) : (as >*< bs)
