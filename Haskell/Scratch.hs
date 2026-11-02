module Equipment where
-- Profunctor equipment toy model

import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Profunctor
    ( Costar(Costar), Star(Star), Profunctor(dimap, lmap, rmap) )
import Data.Profunctor.Composition ( Procompose(..) )

{-
data Procompose p q d c where
  Procompose :: p x c -> q d x -> Procompose p q d c
-}

-- 2-cell as a natural transformation
type Cell f g h j = forall a c . h a c -> j (f a) (g c)

-- Horizontal composition of 2-cells
hcomp :: (Functor f, Functor f', Functor g, Functor g'
         , Profunctor h, Profunctor j, Profunctor k) =>
  Cell f g h j -> Cell f' g' j k -> Cell (Compose f' f) (Compose g' g) h k
hcomp fg_hj fg_jk hac = dimap getCompose Compose $ fg_jk (fg_hj hac)

-- Vertical composition of 2-cells
vcomp :: (Functor f, Functor g, Functor h
         , Profunctor p, Profunctor q, Profunctor r, Profunctor s) =>
  Cell f g p r -> Cell g h q s -> Cell f h (Procompose q p) (Procompose s r)
vcomp fg_pr gh_qs (Procompose qxc pax) = Procompose (gh_qs qxc) (fg_pr pax)

-- Horizontal unit cell
type Hunit p = Cell Identity Identity p p

hUnit :: Profunctor p => Hunit p
hUnit = dimap runIdentity Identity

-- Vertical unit cell
type Vunit f a b = Cell f f (->) (->)

vUnit :: Functor f => Vunit f a b
vUnit = fmap

-- newtype Star   f d c = Star   { runStar   :: d -> f c }
-- newtype Costar f d c = Costar { runCostar :: f d -> c }

type Companion f d c = Costar f d c

type Conjoint f d c  = Star f d c

type CompUnit f   = Cell Identity f (->) (Costar f)

compUnit :: Functor f => CompUnit f
compUnit h = Costar (fmap (h . runIdentity))

type CompCoUnit f = Cell f Identity (Costar f) (->)

compCoUnit :: Functor f => CompCoUnit f
compCoUnit (Costar h) = Identity . h

type ConjUnit f   = Cell f Identity (->) (Star f)

conjUnit :: Functor f => ConjUnit f
conjUnit h = Star  (fmap (Identity . h))

type ConjCoUnit f = Cell Identity f (Star f) (->)

conjCoUnit :: Functor f => ConjCoUnit f
conjCoUnit (Star h) = h . runIdentity

--------------------------------------------
-- Right adjoints to profunctor composition
-- Since composition is not symmetric, there
-- are two different closures
--------------------------------------------

-- Right closure

newtype (q -|> r) a b = RightClosure (forall x. q x a -> r x b)

instance (Profunctor p, Profunctor q) => Profunctor (p -|> q) where
  dimap f g (RightClosure qp) = RightClosure (rmap g . qp . rmap f)

-- curry
rightAdjR :: (Profunctor p, Profunctor q, Profunctor r) =>
  (forall a b . Procompose p q a b -> r a b) -> 
  (forall a b . p a b              -> (q -|> r) a b)
rightAdjR pq_r pab = RightClosure (pq_r . Procompose pab)

-- uncurry
leftAdjR :: (Profunctor p, Profunctor q, Profunctor r) =>
  (forall a b . p a b -> (q -|> r) a b) -> 
  (forall a b . Procompose p q a b -> r a b)
leftAdjR p_arr (Procompose pxb qax) =
  let RightClosure q_r = p_arr pxb
  in  q_r qax

unitR :: (Profunctor p, Profunctor q) =>
   forall a b . p a b -> (q -|> Procompose p q) a b
unitR pab = RightClosure (Procompose pab)

counitR :: (Profunctor p, Profunctor q) =>
    forall a b. (Procompose (p -|> q) p) a b -> q a b
counitR (Procompose (RightClosure pq) pyb) = pq pyb

-- Left closure

newtype (r <|- p) a b = LeftClosure  (forall x. p b x -> r a x)

instance (Profunctor p, Profunctor q) => Profunctor (p <|- q) where
  dimap f g (LeftClosure pq) = LeftClosure (lmap f . pq . lmap g)

-- curry
rightAdjL :: (Profunctor p, Profunctor q, Profunctor r) =>
  (forall a b . Procompose p q a b -> r a b) -> 
  (forall a b . q a b              -> (r <|- p) a b)
rightAdjL pq_r qab = LeftClosure (\pbx -> pq_r (Procompose pbx qab))

-- uncurry
leftAdjL :: (Profunctor p, Profunctor q, Profunctor r) =>
  (forall a b . q a b -> (r <|- p) a b) -> 
  (forall a b . Procompose p q a b -> r a b)
leftAdjL q_arr (Procompose pxb qax) =
  let LeftClosure p_r = q_arr qax
  in  p_r pxb

unitL :: (Profunctor p, Profunctor q) =>
   forall a b . q a b -> (Procompose p q <|- p) a b
unitL qab = LeftClosure (\pbx -> Procompose pbx qab)

counitL :: (Profunctor p, Profunctor q) =>
    forall a b. (Procompose p (q <|- p)) a b -> q a b
counitL (Procompose pxb (LeftClosure pq)) = pq pxb


data Cart f g a b = Cart (f a -> g b)

instance (Functor f, Functor g) => Profunctor (Cart f g) where
  dimap a'a bb' (Cart h) = Cart ( fmap bb' . h . fmap a'a)

-- Right Kan extension in a profunctor equipment

data Ran' j d a where
  Ran' :: (forall b . j a b -> d b) -> Ran' j d a


instance (Profunctor j, Functor d) => Functor (Ran' j d) where
  fmap f (Ran' j_d) = Ran' (j_d . lmap f)

epsilon' :: (Profunctor j, Functor d) => Cell (Ran' j d) d j (->)
epsilon' jab (Ran' ran) = ran jab

type Phi s d j h = Cell s d (Procompose j h) (->)

type Phi' s d j h = Cell s (Ran' j d) h (->)

phi'' :: (Profunctor j, Profunctor h, Functor d, Functor s) =>
        Phi s d j h -> Phi' s d j h
phi'' phi hac sa = Ran' (\ jcb -> phi (Procompose jcb hac) sa)


phi''' :: (Profunctor j, Profunctor h, Functor d, Functor s) =>
          Phi' s d j h -> Phi s d j h
phi''' phi' (Procompose jxc hax) sa =
  let (Ran' ran) = phi' hax sa
  in  ran jxc

-- phi' :: Cell s (Ran' j d) h (->)
  -- h a b -> s a -> (Ran' j d) b
  -- h a b -> s a -> j a x -> d x
{-

vcomp :: (Functor f, Functor g, Functor h
         , Profunctor p, Profunctor q, Profunctor r, Profunctor s) =>
  Cell f g p r -> Cell g h q s -> Cell f h (Procompose q p) (Procompose s r)
vcomp fg_pr gh_qs (Procompose qxc pax) = Procompose (gh_qs qxc) (fg_pr pax)


phi'' :: (Profunctor j, Profunctor h, Functor d, Functor s) =>
          Cell s d (Procompose j h) (->)
       -> Cell s (Ran' j d) h (->)
phi'' phi has sa = Ran' (\ jcb -> _) --  -> d b
-}
-----

-- original definition

newtype Ran0 j d a = Ran0 (forall e. (a -> j e) -> d e)

counit :: forall j d e'. Ran0 j d (j e') -> d e'
counit (Ran0 h) = h id

-- Universal property

type Alpha j d g = forall e. g (j e) -> d e

sigma :: Functor g => Alpha j d g -> forall b. (g b -> Ran0 j d b)
sigma alpha gb = Ran0 (\b_je -> alpha $ fmap b_je gb)

factorize :: Functor g => Alpha j d g -> forall e. g (j e) -> d e
factorize alpha = counit . sigma alpha

-- By equational reasoning, this is equivalent to
factorize' :: Functor g => Alpha j d g -> forall e. g (j e) -> d e
factorize' alpha = alpha


type Ran j d a = forall b . j a b -> d b

-- the counit of Ran
epsilon :: (Profunctor j, Functor d) => forall a b . j a b -> (Ran j d a -> d b)
epsilon jab ran = ran jab

-- Given another functor s with the same property
-- phi :: forall c b. Procompose j h c b -> (s c -> d b)
-- there is a unique factorization through a 2-cell phi'

-- (coend h x b, j c x) -> s c -> d b
-- 

-- data ProfArrow h (Cart s d) a b where
--   ProfArrow :: (forall x . h b x -> Cart s d a x)  
--  forall x . h b x -> s a -> d x

-- s c' -> d b
