import Prelude hiding (Monad, (>>=), return, Applicative, pure, (<*>))
import GHC.Core.Coercion.Axiom (fsFromRole)

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
  return = pure

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap fs as = fs >>= 
    (\f -> as >>= 
        \a -> return (f a))

instance Monad Maybe where
  Nothing  >>= k = Nothing
  (Just a) >>= k = k a

instance Applicative Maybe where
    pure = Just
    (<*>) = ap

newtype Writer w a = Writer { runWriter :: (a, w) }
  deriving Functor

instance Monoid w => Monad (Writer w) where
  (Writer (a, w)) >>= k = let (Writer (b, w')) = k a
                          in Writer (b, mappend w w')
    
instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    (<*>) = ap

newtype Reader e a = Reader { runReader :: e -> a }
  deriving Functor

instance Monad (Reader e) where
  ma >>= k = Reader (\e -> let a = runReader ma e
                           in runReader (k a) e)

instance Applicative (Reader e) where
    pure a = Reader (const a)
    (<*>) = ap

newtype State s a = State { runState :: s -> (a, s) }
    deriving Functor

instance Monad (State s) where
  st >>= k = State (\s -> let (a, s') = runState st s
                          in runState (k a) s')

instance Applicative (State s) where
    pure a = State (\s -> (a, s))
    (<*>) = ap

join :: State s (State s a) -> State s a
join mma = State (uncurry runState . runState mma)
{- Or, more explicitly
join mma = State (\s -> let (ma, s') = runState mma s
                        in runState ma s')
-}

get :: State s s
get = State (\s -> (s, s))

set :: s -> State s ()
set s = State (\_ -> ((), s))

instance Monad [] where
  as >>= k = concat (fmap k as)

instance Applicative [] where
    pure a = [a]
    (<*>) = ap

newtype Cont r a = Cont { runCont :: (a -> r) -> r }
  deriving Functor

instance Monad (Cont r) where
  ma >>= fk = Cont (\k -> runCont ma (\a -> runCont (fk a) k))

instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure a = Cont (\k -> k a)
    (<*>) = ap

pairs :: [a] -> [b] -> [(a, b)]
pairs as bs = do
  a <- as
  b <- bs
  pure (a, b)

pairs' :: [a] -> [b] -> [(a, b)]
pairs' as bs = [ (a, b) | a <- as, b <- bs ]

main :: IO ()
main = do
  s <- getLine
  if s == "yes"
  then putStrLn "Thank you!"
  else putStrLn "Next time."
