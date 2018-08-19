{-
GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5
[6,8,8,10]
GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
GHCi> getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4
Just 81
-}

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap f = Arr2T . (fmap . fmap . fmap) f . getArr2T

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap f = Arr3T . (fmap . fmap . fmap . fmap) f . getArr3T

instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure x = Arr2T $ \_ _ -> pure x
  (Arr2T f) <*> (Arr2T v) = Arr2T $ \a b -> (f a b) <*> (v a b)

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure x = Arr3T $ \_ _ _ -> pure x
  (Arr3T f) <*> (Arr3T v) = Arr3T $ \a b c -> (f a b c) <*> (v a b c)

instance Monad m => Monad (Arr2T e1 e2 m) where
{-
    (>>=) :: (e1 -> e2 -> m a)
          -> (a -> e1 -> e2 -> m b)
          -> e1 -> e2 -> m b
-}
  (Arr2T f) >>= k = Arr2T $
    \a b -> do
      x <- f a b
      getArr2T (k x) a b

instance Monad m => Monad (Arr3T e1 e2 e3 m) where
  (Arr3T f) >>= k = Arr3T $
    \a b c -> do
      x <- f a b c
      getArr3T (k x) a b c


a2l = Arr2T $ \e1 e2 -> [e1,e2]
a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)

ex1 = getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5
ex2 = getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4
