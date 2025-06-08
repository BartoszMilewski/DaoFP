import Data.List (partition)

type Coalgebra f a = a -> f a

data TreeF x = LeafF | NodeF Int x x
  deriving (Show, Functor)

split :: Coalgebra TreeF [Int]
split [] = LeafF
split (n : ns) = NodeF n left right
  where
    (left, right) = partition (<= n) ns

newtype Fix f = Fix { unFix :: f (Fix f) }

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coa = Fix . fmap (ana coa) . coa 

type Algebra f c = f c -> c

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

toList :: Algebra TreeF [Int]
toList LeafF = []
toList (NodeF n ns ms) = ns ++ [n] ++ ms

qsort = cata toList . ana split

data StreamF a x = StreamF a x
  deriving Functor

type Stream a = Fix (StreamF a)

step :: Coalgebra (StreamF Int) Int
step n = StreamF n (n+1)

allNats :: Stream Int
allNats = ana step 0


hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coa = alg . fmap (hylo alg coa) . coa 

qsort' = hylo toList split

add :: Algebra (StreamF Int) Int
add (StreamF n sum) = n + sum

-- Runs forever
sumAllNats :: Int
sumAllNats = hylo add step 1

ana' :: Functor f => forall a. Coalgebra f a -> (a -> Fix f)
ana' coa = Fix . fmap (ana' coa) . coa 

ana'' :: Functor f => forall a. (a -> f a, a) -> Fix f
ana'' (coa, x) = Fix (fmap (curry ana'' coa) (coa x))

newtype Mu f = Mu (forall a. Algebra f a -> a)
-- data Nu f = Nu (exists a. (Coalgebra f a, a))
data Nu f where 
  Nu :: (a -> f a, a) -> Nu f

anaNu :: Coalgebra f a -> a -> Nu f
anaNu coa a = Nu (coa, a)

head :: Nu (StreamF a) -> a
head (Nu (unf, s)) = 
  let (StreamF a _) = unf s 
  in a

tail :: Nu (StreamF a) -> Nu (StreamF a)
tail (Nu (unf, s)) = 
  let (StreamF _ s') = unf s 
  in Nu (unf, s')

nuArgs :: (Int -> StreamF Int Int, Int)
nuArgs =  (\n -> StreamF n (n+1) , 0)

allNats' = Nu nuArgs
