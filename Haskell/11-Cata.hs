data ExprF x = ValF Int | PlusF x x
    deriving Functor

data Fix' f where
  In' :: f (Fix' f) -> Fix' f

out' :: Fix' f -> f (Fix' f)
out' (In' x) = x

newtype Fix f = Fix { unFix :: f (Fix f) }

val :: Int -> Fix ExprF
val n = Fix (ValF n)

plus :: Fix ExprF -> Fix ExprF -> Fix ExprF
plus e1 e2 = Fix (PlusF e1 e2)

e9 :: Fix ExprF
e9 = plus (plus (val 2) (val 3)) (val 4)

type Algebra f c = f c -> c

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

eval :: Algebra ExprF Int
eval (ValF n)   = n
eval (PlusF m n) = m + n

pretty :: Algebra ExprF String
pretty (ValF n)   = show n
pretty (PlusF s t) = s ++ " + " ++ t

test1 = cata eval e9
test2 = cata pretty e9

pretty' :: Algebra ExprF (Int -> String)
pretty' (ValF n) i = indent i ++ show n
pretty' (PlusF f g) i = f (i + 1) ++ "\n" ++
                        indent i ++ "+" ++ "\n" ++
                        g (i + 1)

indent n = replicate (n * 2) ' '

main = putStrLn (cata pretty' e9 0)

data ListF a x = NilF | ConsF a x
    deriving Functor

type List a = Fix (ListF a)

{-
alg :: ListF a c -> c
alg NilF = init
alg (ConsF a c) = step (a, c)

init :: c
step :: (a, c) -> c

recList :: c -> ((a, c) -> c) -> (List a -> c)
-}

recList :: forall c a . c -> ((a, c) -> c) -> (List a -> c)
recList init step = cata alg
  where
    alg :: ListF a c -> c
    alg NilF = init
    alg (ConsF a c) = step (a, c)
   
revAlg :: Algebra (ListF a) ([a]->[a])
revAlg NilF = id
revAlg (ConsF a f) = \as -> f (a : as)

reverse :: Fix (ListF a) -> [a]
reverse as = (cata revAlg as) []
