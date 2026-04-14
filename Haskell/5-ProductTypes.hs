
thrd :: (a, b, c) -> c
thrd (_, _, c) = c

data Product a b = Pair { fst' :: a, snd' :: b }

ic :: Product Int Char
ic = Pair 10 'A'

swap' :: (a, b) -> (b, a)
swap' x = (snd x, fst x)

swap (x, y) = (y, x)

assoc :: ((a, b), c) -> (a, (b, c))
assoc ((a, b), c) = (a, (b, c))

runit :: (a, ()) -> a
runit (a, _) = a

class Monoid' m where
  mappend' :: (m, m) -> m
  mempty'  :: () -> m

-- class Semigroup a where
--    (<>) :: a -> a -> a

newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a) where
  (Endo f) <> (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

-- Exercises

maybeAB :: Either b (a, b) -> (Maybe a, b)
maybeAB (Left b) = (Nothing, b)
maybeAB (Right (a, b)) = (Just a, b)
-- Another possibility:
-- maybeAB (Right (a, b)) = (Nothing, b)

newtype All = All Bool

instance Semigroup All where
  (All True) <> (All True) = All True
  _ <> _ = All False

instance Monoid All where
  mempty = All True

newtype Any = Any Bool

instance Semigroup Any where
  (Any a) <> (Any b) = Any (a || b)
  _ <> _ = Any True

instance Monoid Any where
  mempty = Any False
