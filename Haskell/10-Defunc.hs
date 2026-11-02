sumK :: [Int] -> (Int -> r) -> r
sumK [] k = k 0
sumK (i : is) k =
  sumK is (\s -> k (i + s))

sumList :: [Int] -> Int
sumList as = sumK as (\i -> i)

-- Replace lambda closures with named functions
-- \s -> k (i + s)
more :: (Int, Int -> r) -> Int -> r
more (i, k) s = k (i + s)

done :: r -> r
done i = i

sumK' :: [Int] -> (Int -> r) -> r
sumK' [] k = k 0
sumK' (i : is) k =
  sumK' is (more (i, k))

sumList' :: [Int] -> Int
sumList' is = sumK' is done

data Comma a b e = Comma e ((e, a) -> b)

-- Abstract the environments 
data Kont = Done           -- empty environment
          | More Int Kont

-- apply :: (a -> b, a) -> b
-- apply (f, x) = f x

apply :: (Kont, Int) -> Int
apply (Done, i) = i
apply (More i k, s) = apply (k, i + s)

sumK'' :: [Int] -> Kont -> Int
sumK'' [] k = apply (k, 0)
sumK'' (i : is) k = sumK'' is (More i k)

sumList'' :: [Int] -> Int
sumList'' is = sumK'' is Done

-- Exercise: defunctionalize tree traversal

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)

-- Original implementation
maxTree :: Tree Int -> Int
maxTree (Leaf n) = n
maxTree (Node t1 t2) = max (maxTree t1) (maxTree t2)

-- CPS transform
maxTreeK :: Tree Int -> (Int -> r) -> r
maxTreeK (Leaf n) k = k n
maxTreeK (Node t1 t2) k = maxTreeK t1    -- traverse left
  (\n1 -> maxTreeK t2                    -- traverse right
    (\n2 -> k (max n1 n2)))              -- combine

maxT :: Tree Int -> Int
maxT t = maxTreeK t id

-- Replace lambda closures with named functions

-- \n1 -> maxTreeK t2 (\n2 -> k (max n1 n2))
left :: (Tree Int, Int -> r) -> Int -> r
left (t2, k) n1 = maxTreeK' t2 (right (n1, k))

-- \n2 -> k (max n1 n2))
right :: (Int, Int -> r) -> Int -> r
right (n1, k) n2 = k (max n1 n2)

doneT :: r -> r
doneT = id

maxTreeK' :: Tree Int -> (Int -> r) -> r 
maxTreeK' (Leaf n)     k = k n 
maxTreeK' (Node t1 t2) k = maxTreeK' t1 (left (t2, k))

maxT' :: Tree Int -> Int
maxT' t = maxTreeK' t doneT

-- Abstract the environments

data KontT = DoneT 
           | LeftT  (Tree Int) KontT  -- push right subtree
           | RightT Int        KontT  -- push left max

{- As a stack, it's a list of (Either (Tree Int) Int)
data KontT = DoneT
           | ConsT (Either (Tree Int) Int) KontT
-}

applyT :: (KontT, Int) -> Int
applyT (DoneT, n) = n
applyT (LeftT  t1 k, n1) = maxTreeK'' t1 (RightT n1 k)
applyT (RightT n1 k, n2) = applyT (k, max n1 n2)

maxTreeK'' :: Tree Int -> KontT -> Int
maxTreeK'' (Leaf n) k     = applyT (k, n)
maxTreeK'' (Node t1 t2) k = maxTreeK'' t1 (LeftT t2 k)

maxT'' :: Tree Int -> Int
maxT'' t = maxTreeK'' t DoneT

test :: Int
test = maxT'' t

t :: Tree Int
t = Node 
      (Node (Leaf 0) (Leaf 5)) 
      (Node (Leaf 14) (Node (Leaf 3) (Leaf 7)))
