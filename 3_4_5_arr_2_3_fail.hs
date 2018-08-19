{-
GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
GHCi> getArr3T (do {9 <- a3m; y <- a3m; return y}) 2 3 4
Just 9
GHCi> getArr3T (do {10 <- a3m; y <- a3m; return y}) 2 3 4
Nothing
GHCi> a2m = Arr2T $ \e1 e2 -> Just (e1 + e2)
GHCi> getArr2T (do {5 <- a2m; y <- a2m; return y}) 2 3
Just 5
GHCi> getArr2T (do {6 <- a2m; y <- a2m; return y}) 2 3
Nothing
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
  (Arr2T f) >>= k = Arr2T $
    \a b -> do
      x <- f a b
      getArr2T (k x) a b
  fail s = Arr2T $ \_ _ -> fail s

instance Monad m => Monad (Arr3T e1 e2 e3 m) where
  (Arr3T f) >>= k = Arr3T $
    \a b c -> do
      x <- f a b c
      getArr3T (k x) a b c
  fail s = Arr3T $ \_ _ _ -> fail s

a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
ex1 = getArr3T (do {9 <- a3m; y <- a3m; return y}) 2 3 4
ex2 = getArr3T (do {10 <- a3m; y <- a3m; return y}) 2 3 4

a2m = Arr2T $ \e1 e2 -> Just (e1 + e2)
ex3 = getArr2T (do {5 <- a2m; y <- a2m; return y}) 2 3
ex4 = getArr2T (do {6 <- a2m; y <- a2m; return y}) 2 3

