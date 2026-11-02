import Control.Monad (ap)

data Ex x = Val Int 
          | Var x 
          | Plus (Ex x) (Ex x) 
 deriving (Functor, Show)

ex :: Ex Char
ex = Plus (Plus (Val 2) (Var 'a')) (Var 'b')

instance Monad Ex where
  Val n >>= k = Val n
  Var x >>= k = k x
  Plus e1 e2 >>= k = 
    let x = e1 >>= k
        y = e2 >>= k
    in (Plus x y)

instance Applicative Ex where 
  pure x = Var x 
  (<*>) = ap

sub :: Char -> Ex String
sub 'a' = Plus (Var "x1") (Val 2)
sub 'b' = Var "x2"

ex' :: Ex String
ex' = ex >>= sub

main :: IO ()
main = print ex'