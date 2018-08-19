{-
GHCi> (getArr2T $ arr2 (+)) 33 9 :: [Integer]
[42]
GHCi> (getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer
Right 120
GHCi> import Data.Functor.Identity
GHCi> runIdentity $ (getArr2T $ arr2 (+)) 33 9
42
-}

import Data.Functor.Identity

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

{-

instance Functor (-> e) where
  fmap :: (a -> b) -> (e -> a) -> (e -> b)
  fmap f g = \x -> f (g x)
  fmap f g = f . g
  fmap = (.)

-}

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
-- arr2 f = Arr2T $ (return .) . f
arr2 = Arr2T . ((return <$>) <$>)
{-
fmap return :: f a -> f (m a) == (e -> a) -> (e -> m a)
fmap (fmap return) :: f1 (f2 a) -> f1 (f2 (m a) ==
  (e1 -> (e2 -> a)) -> (e1 -> (e2 -> m a)) ==
  (e1 -> e2 -> a) -> (e1 -> e2 -> m a) ==
  (e1 -> e2 -> a) -> e1 -> e2 -> m a
-}

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
-- arr3 f = Arr3T $ ((return .) .) . f
arr3 = Arr3T . fmap (fmap (fmap return))
{-
fmap (fmap (fmap return)) :: f1 (f2 (f3 a)) -> f1 (f2 (f3 (m a))) ==
  (e1 -> (e2 -> (e3 -> a))) -> (e1 -> (e2 -> (e3 -> m a)) ==
  (e1 -> e2 -> e3 -> a) -> e1 -> e2 -> e3 -> m a
-}

ex1 = (getArr2T $ arr2 (+)) 33 9 :: [Integer]
ex2 = (getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer
ex3 = runIdentity $ (getArr2T $ arr2 (+)) 33 9

