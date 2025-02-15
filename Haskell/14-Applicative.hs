import Prelude hiding (Applicative, pure, (<*>), (<$>), (*>), (<*), zipWith, repeat)

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

(*>) :: Applicative f => f a -> f b -> f b
u *> v = (\ _ x -> x) <$> u <*> v

-- all these operators bind to the left 
-- and have precedence 4 
-- (on the scale of, lowest to highest, 0 to 9)
infixl 4 <*>
infixl 4 <$>
infixl 4 *>

instance Applicative Maybe where
  pure = Just
  mf <*> ma =
    case (mf, ma) of
      (Just f, Just a) -> Just (f a)
      _ -> Nothing

data Validation e a
  = Failure e
  | Success a

instance Functor (Validation e) where
   fmap _ (Failure e) = Failure e
   fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (mappend e1 e2)
  Failure e  <*> Success _ = Failure e
  Success _  <*> Failure e  = Failure  e
  Success f  <*> Success x  = Success (f x)


newtype Writer w a = Writer { runWriter :: (a, w) }
  deriving Functor

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  wf <*> wa = let (f, w)  = runWriter wf
                  (a, w') = runWriter wa
              in Writer (f a, mappend w w')

newtype Reader e a = Reader { runReader :: e -> a }
  deriving Functor

instance Applicative (Reader e) where
  pure a = Reader (const a)
  rf <*> ra = Reader (\e -> (runReader rf e) (runReader ra e))

newtype State s a = State { runState :: s -> (a, s) }
  deriving Functor

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  sf <*> sa = State (\s ->
    let (f, s') = runState sf s
        (a, s'') = runState sa s'
    in (f a, s''))

newtype ZipList a = ZipList { unZip :: [a] }
  deriving Functor

instance Applicative ZipList where
  pure a = ZipList (repeat a)
  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  zf <*> za = ZipList (zipWith ($) (unZip zf) (unZip za))

repeat :: a -> [a]
repeat a = a : repeat a

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (a : as) (b : bs) = f a b : zipWith f as bs

instance Applicative [] where
  pure a = [a]
  fs <*> as = [ f a | f <- fs, a <- as ]


newtype Cont r a = Cont { runCont :: (a -> r) -> r }
  deriving Functor

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont (\k -> k a)
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  kf <*> ka = Cont (\k ->
        runCont kf (\f ->
        runCont ka (\a -> k (f a))))
  -- k  :: b -> r

newtype Compose f g a = Compose (f(g a))
  deriving Functor

instance (Applicative f, Applicative g) => Applicative (Compose g f) where
    pure :: a -> Compose g f a
    pure x = Compose (pure (pure x))
    (<*>) :: Compose g f (a -> b) -> Compose g f a -> Compose g f b
    Compose gff <*> Compose gfa = Compose (fmap (<*>) gff <*> gfa)
