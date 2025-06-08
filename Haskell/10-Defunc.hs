import GHC.CmmToAsm.AArch64.Instr (x0)
sumK :: [Int] -> (Int -> r) -> r
sumK [] k = k 0
sumK (i : is) k =
  sumK is (\s -> k (i + s))

sumList :: [Int] -> Int
sumList as = sumK as (\i -> i)

more :: (Int, Int -> r) -> Int -> r
more (i, k) s = k (i + s)

done :: Int -> Int
done i = i

sumK' :: [Int] -> (Int -> r) -> r
sumK' [] k = k 0
sumK' (i : is) k =
  sumK' is (more (i, k))

sumList' :: [Int] -> Int
sumList' is = sumK' is done

data Comma a b e = Comma e ((e, a) -> b)

data Kont = Done | More Int Kont

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
