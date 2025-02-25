sum2 = go 0 
  where go n [] = n
        go n (i : is) = go (n + i) is

data Tree = Leaf String 
          | Node Tree String Tree

show' :: Tree -> String
show' (Node lft s rgt) =
  let ls = show' lft
      rs = show' rgt
  in ls ++ s ++ rs

showk :: Tree -> Cont r String
showk (Leaf s) = pure s
showk (Node lft s rgt) = do
  ls <- showk lft
  rs <- showk rgt
  pure (ls ++ s ++ rs)
