data Expr = Val Int 
          | Plus Expr Expr

e2 = Val 2
e3 = Val 3
e5 = Plus e2 e3
e7 = Plus e5 e2

data ExprF x = ValF Int 
             | PlusF x x

{-
eval :: ExprF Int -> Int
eval (ValF n)    = n
eval (PlusF m n) = m + n

pretty :: ExprF String -> String
pretty (ValF n)    = show n
pretty (PlusF s t) = s ++ " + " ++ t
-}

type Algebra f c = f c -> c

eval :: Algebra ExprF Int
eval (ValF n)   = n
eval (PlusF m n) = m + n

pretty :: Algebra ExprF String
pretty (ValF n)   = show n
pretty (PlusF s t) = s ++ " + " ++ t

-- Exercise
data FloatF x = Num Float | Op x x
    deriving Functor

addAlg :: Algebra FloatF Float
addAlg (Num x) = log x
addAlg (Op x y) = x + y

mulAlg :: Algebra FloatF Float
mulAlg (Num x) = x
mulAlg (Op x y) = x * y

-- Two paths through the algebra morphism diagram

logMul :: FloatF Float -> Float
logMul = log . mulAlg

addLog :: FloatF Float -> Float 
addLog = addAlg . fmap log

x :: FloatF Float
x = Op 2.3 13.1

test1 = logMul x
test2 = addLog x