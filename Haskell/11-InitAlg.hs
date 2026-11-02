newtype Fix f = Fix { unFix :: f (Fix f) }

type Algebra f c = f c -> c

cata :: Functor f => forall a. Algebra f a -> Fix f -> a
--cata alg = alg . fmap (cata alg) . unFix
cata alg (Fix x) = alg (fmap (cata alg) x)

cata' :: Functor f => Fix f -> forall a. (Algebra f a -> a)
cata' (Fix x) = \alg -> alg (fmap (flip' cata' alg) x)
  where
    flip' :: (c -> forall a. Algebra f a -> a) -> (forall a. Algebra f a -> c -> a)
    flip' g a c = g c a

uncata :: Functor f => (forall a. Algebra f a -> a) -> Fix f
uncata alga = alga Fix

newtype Mu f = Mu (forall a. Algebra f a -> a)

cataMu :: Algebra f a -> (Mu f -> a)
cataMu alg (Mu h) = h alg

data ListF a x = NilF | ConsF a x
    deriving Functor

fromList :: forall a. [a] -> Mu (ListF a)
fromList as = Mu h
  where h :: forall x. Algebra (ListF a) x -> x
        h alg = go as
          where
            go [] = alg NilF
            go (n: ns) = alg (ConsF n (go ns))

-- Exercise

sumAlg :: Algebra (ListF Int) Int
sumAlg NilF = 0
sumAlg (ConsF n x) = n + x 

sumList :: [Int] -> Int
sumList = cataMu sumAlg . fromList 