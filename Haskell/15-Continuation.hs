import Control.Monad

newtype Cont r a = Cont { runCont :: (a -> r) -> r }
  deriving Functor

instance Monad (Cont r) where
  ma >>= fk = Cont (\k -> runCont ma (\a -> runCont (fk a) k))

instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure a = Cont (\k -> k a)
    (<*>) = ap

sum2 = go 0 
  where go n [] = n
        go n (i : is) = go (n + i) is

data Tree = Leaf String 
          | Node Tree String Tree

tree :: Tree
tree = Node (Leaf "leaf ") "root " (Node (Leaf "l1 ") "right " (Leaf "l2 "))
show1 :: Tree -> String
show1 (Node lft s rgt) =
  let ls = show1 lft
      rs = show1 rgt
  in ls ++ s ++ rs

showk2 :: Tree -> Cont r String
showk2 (Leaf s) = pure s
showk2 (Node lft s rgt) = do
  ls <- showk2 lft
  rs <- showk2 rgt
  pure (ls ++ s ++ rs)

show2 :: Tree -> String
show2 t = runCont (showk2 t) id

showk3 :: Tree -> (String -> r) -> r
showk3 (Leaf s) k = k s
showk3 (Node lft s rgt) k =
  showk3 lft (\ls -> 
    showk3 rgt (\rs -> 
      k (ls ++ s ++ rs)))

show3 :: Tree -> String
show3 t = runCont' (showk3 t) id
  where runCont' cont k = cont k


-- Using named functions

showk4 :: Tree -> (String -> r) -> r
showk4 (Leaf s) k = k s
showk4 (Node lft s rgt) k =
  showk4 lft (next (s, rgt, k))

next :: (String, Tree, String -> r) -> String -> r
next (s, rgt, k) ls = showk4 rgt (conc (ls, s, k))

conc :: (String, String, String -> r) -> String -> r
conc (ls, s, k) rs = k (ls ++ s ++ rs)

done :: String -> String
done s = s

show4 t = showk4 t done

-- Defunctionalization

data Kont = Done 
          | Next String Tree Kont 
          | Conc String String Kont

apply :: (Kont, String) -> String
apply (Done, s) = s
apply (Next s rgt k, ls) = showk rgt (Conc ls s k)
apply (Conc ls s k, rs) = apply (k, ls ++ s ++ rs)

showk :: Tree -> Kont -> String
showk (Leaf s) k = apply (k, s)
showk (Node lft s rgt) k = showk lft (Next s rgt k)

showTree t = showk t Done

main :: IO ()
main = putStrLn (showTree tree)