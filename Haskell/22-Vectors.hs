{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

import Data.Kind (Type)
import GHC.Plugins (Var(varName))

data Nat = Z | S Nat

data Vec' n a where
    VNil' :: Vec' Z a
    VCons' :: a -> Vec' n a -> Vec' (S n) a

data Vec (n :: Nat) (a :: Type) where
    VNil  :: Vec Z a
    VCons :: a -> Vec n a -> Vec (S n) a

emptyV :: Vec Z Int
emptyV = VNil

singleV :: Vec (S Z) Int
singleV = VCons 42 VNil

headV :: Vec (S n) a -> a
headV (VCons a _) = a

zipV :: Vec n a -> Vec n b -> Vec n (a, b)
zipV (VCons a as) (VCons b bs) = VCons (a, b) (zipV as bs)
zipV VNil VNil = VNil

-- Exercise
tailV :: Vec (S n) a -> Vec n a
tailV (VCons _ v) = v

-- bad = tailV VNil

sumV :: Vec n Int -> Int
sumV VNil = 0
sumV (VCons n v) = n + sumV v

-- Proposed notation for sigma and pi types
-- some (a :: A) | F a
-- forall (a :: A) -> F a

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

replicateV :: a -> SNat n -> Vec n a
replicateV _ SZ  = VNil
replicateV x (SS n) = VCons x (replicateV x n)

equal :: Nat -> Nat -> Bool
equal Z Z = True
equal (S m) (S n) = equal m n
equal _ _ = False

add :: Nat -> Nat -> Nat
add n Z = n
add n (S m) = S (add n m)

filterV :: (a -> Bool) -> Vec n a -> (forall k. Vec k a -> r) -> r 
filterV p VNil k = k VNil
filterV p (VCons a as) k = 
  if p a 
     then filterV p as (\bs -> k (VCons a bs))
     else filterV p as (\bs -> k bs)

toList :: Vec n a -> [a]
toList VNil = []
toList (VCons a as) = a : toList as

v :: Vec (S (S (S (S Z)))) Int
v = VCons 2 (VCons (-2) (VCons 3 (VCons (-1) VNil)))

test :: [Int]
test = filterV (> 0) v toList