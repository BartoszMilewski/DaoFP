import Control.Comonad

data Stream a = Cons a (Stream a)
    deriving Functor

instance Comonad Stream where
  extract (Cons a as) = a
  -- either one is enough
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)
  extend f (Cons a as) = Cons (f (Cons a as)) (extend f as)

avg :: Stream Double -> Double
avg  = (/5). sum . stmTake 5

stmTake :: Int -> Stream a -> [a]
stmTake 0 _ = []
stmTake n (Cons a as) = a : stmTake (n - 1) as

smooth :: Stream Double -> Stream Double
smooth = extend avg

data Signal a = Sig (Double -> a) Double
    deriving Functor

instance Comonad Signal where
  extract (Sig f x) = f x
  duplicate (Sig f x) = Sig (\y -> Sig f (x - y)) x
  extend g (Sig f x) = Sig (\y -> g (Sig f (x - y))) x

-- Exercise
data BiStream a = BStr [a] [a]
    deriving Functor

fwd :: BiStream a -> BiStream a
fwd (BStr as (b : bs)) = 
    BStr (b : as) bs

bwd :: BiStream a -> BiStream a
bwd (BStr (a : as) bs) =
    BStr as (a : bs)

instance Comonad BiStream where
    extract (BStr _ (a : as)) = a
    duplicate bstr = BStr (iterate bwd (bwd bstr)) (iterate fwd bstr)

takeBStr :: Int -> BiStream a -> [a]
takeBStr n (BStr past futr)= reverse (take n past) ++ take (n + 1) futr

bstr = BStr [-1, -2 ..] [0 ..]

test = fmap (takeBStr 2) $ takeBStr 2 (duplicate bstr)

-- Exercise
lowPass :: BiStream Double -> BiStream Double
lowPass = extend avg
  where avg (BStr (a : as) (b0 : b1 : bs)) = (a + b0 + b1) / 3.0 

sig = BStr [1.0, 2.0 ..] [0.0, 1.0 ..]

test2 = takeBStr 3 $ lowPass sig 

class Comonoid w where
  split   :: w -> (w, w)
  destroy :: w -> ()

instance Comonoid w where
  split w   = (w, w)
  destroy w = ()

f x = let (x1, x2) = split x 
      in x1 + x2
g y = let () = destroy y 
      in 42

data Store s c where
    St :: (s -> c) -> s -> Store s c

instance Functor (Store s) where
  fmap g (St f s) = St (g . f) s
