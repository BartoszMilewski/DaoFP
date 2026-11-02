import Prelude hiding (Monoid, mappend, mempty, (++), foldMap)

class Monoid m where
  mappend :: m -> m -> m
  mempty  :: m

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

instance Monoid [a] where
  mempty = []
  mappend = (++)

foldMap :: Monoid m => (a -> m) -> ([a] -> m)
foldMap f = foldr mappend mempty . fmap f

-- Exercise

newtype IntPlus = IntPlus { unPlus :: Int }

instance Monoid IntPlus where
    mempty = IntPlus 0
    mappend (IntPlus x) (IntPlus y) = IntPlus (x + y)

newtype IntTimes = IntTimes { unTimes :: Int }

instance Monoid IntTimes where
    mempty = IntTimes 1
    mappend (IntTimes x) (IntTimes y) = IntTimes (x * y)

sumL :: [Int] -> Int
sumL = unPlus . foldMap IntPlus

multL :: [Int] -> Int
multL = unTimes . foldMap IntTimes