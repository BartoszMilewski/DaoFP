import Control.Monad (ap)
data Id a = MakeId { getId :: a }
    deriving Functor

instance Applicative Id where
    (<*>) = ap
    pure = MakeId 

instance Monad Id where
    ma >>= k = k (getId ma)
