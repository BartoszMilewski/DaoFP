{-# language FunctionalDependencies #-}

class (Functor left, Functor right) => Adjunction1 left right where
  ltor :: (left x -> y) -> (x -> right y)
  rtol :: (x -> right y) -> (left x -> y)

class (Functor left, Functor right) => 
  Adjunction left right | left -> right, right -> left where
    unit   :: x -> right (left x)
    counit :: left (right x) -> x

newtype L r x = L (x, r)    deriving (Functor, Show)
newtype R r x = R (r -> x)  deriving Functor

instance Adjunction (L r) (R r) where
  unit x = R (\r -> L (x, r)) 
  counit (L (R f, r)) = f r

triangle :: L r x -> L r x
triangle = counit . fmap unit

triangle' :: R r x -> R r x
triangle' = fmap counit . unit

-- Exercise
test :: L Char Integer
test = triangle (L (2, 'a'))


test' :: Int -> Int
test' n = let R f = triangle' (R (\x -> x^2))
          in f n