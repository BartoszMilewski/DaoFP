class Monoidal f where
  unit  :: f ()
  (>*<) :: f a -> f b -> f (a, b)

pairWith :: Int -> (String -> (Int, String))
pairWith a = \x -> (a, x)

pairWith' :: Int -> [String -> (Int, String)]
pairWith' a = [\x -> (a, x)]

strength :: Functor f => (e, f a) -> f (e, a)
strength (e, as) = fmap (e, ) as

-- Validation

data Validation e a
  = Failure e
  | Success a

instance Functor (Validation e) where
   fmap _ (Failure e) = Failure e
   fmap f (Success a) = Success (f a)

instance Applicative (Validation e) where
  pure = Success
  Success _  <*> Failure e  = Failure  e
  Success f  <*> Success x  = Success (f x)

-- Exercises
instance Monoidal [] where
  unit :: [()]
  unit = () : unit
  (>*<) :: [a] -> [b] -> [(a, b)]
  as >*< [] = []
  [] >*< bs = []
  (a : as) >*< (b : bs) = (a, b) : (as >*< bs)
