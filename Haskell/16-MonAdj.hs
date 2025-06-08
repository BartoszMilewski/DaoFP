{-# language FunctionalDependencies #-}

-- String diagrams
-- Some random functors
type F a = Int -> a
type G a = [a]
type H a = Maybe a

alpha :: forall x. F x -> G x
alpha f = fmap f [0, 1, 2]

beta :: forall x. G x -> H x
beta [] = Nothing
beta (a : as) = Just a

beta_alpha :: forall x. F x -> H x
beta_alpha = beta . alpha


-- Exercise

class (Functor left, Functor right) => Adjunction1 left right where
  ltor :: (left x -> y) -> (x -> right y)
  rtol :: (x -> right y) -> (left x -> y)

class (Functor left, Functor right) => 
  Adjunction left right | left -> right, right -> left where
    unit   :: x -> right (left x)
    counit :: left (right x) -> x


