class Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> (p a b -> p s t)

data Coend p where
  Coend ::  p x x -> Coend p

newtype ProPair q p a b y x = ProPair (q a x, p y b)

instance (Profunctor p, Profunctor q) => Profunctor (ProPair q p a b) where
    dimap :: (Profunctor p, Profunctor q) => (y' -> y)-> (x -> x') -> 
              ProPair q p a b y x -> ProPair q p a b y' x'
    dimap l r (ProPair (qax, pyb)) = ProPair (dimap id r qax, dimap l id pyb)

newtype CoEndCompose p q a b = CoEndCompose (Coend (ProPair q p a b))

instance (Profunctor p, Profunctor q) => Profunctor (CoEndCompose p q) where
    dimap :: (Profunctor p, Profunctor q) => (s -> a) -> (b -> t) -> 
              CoEndCompose p q a b -> CoEndCompose p q s t
    dimap l r (CoEndCompose (Coend (ProPair (qax, pyb)))) = 
        CoEndCompose (Coend (ProPair (dimap l id qax, dimap id r pyb)))

