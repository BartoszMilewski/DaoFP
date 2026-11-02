newtype Writer w a = Writer { runWriter :: (a, w) }

newtype Reader e a = Reader { runReader :: e -> a }

newtype State s a = State { runState :: s -> (a, s) }

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- Exercise

instance Functor (Cont r) where
  -- f :: a -> b
  -- k :: b -> r
  fmap f c = Cont (\k -> runCont c (k . f))
