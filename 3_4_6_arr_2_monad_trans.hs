{-
GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> getArr2T (do {x <- a2l; y <- lift [10,20,30]; return (x+y)}) 3 4
[13,23,33,14,24,34]
GHCi> getArr2T (do {x <- asks2 const; y <- asks2 (flip const); z <- asks2 (,); return (x,y,z)}) 'A' 'B'
('A','B',('A','B'))
-}

import Control.Monad.Trans.Class

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap f = Arr2T . (fmap . fmap . fmap) f . getArr2T

instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure x = Arr2T $ \_ _ -> pure x
  (Arr2T f) <*> (Arr2T v) = Arr2T $ \a b -> (f a b) <*> (v a b)

instance Monad m => Monad (Arr2T e1 e2 m) where
  (Arr2T f) >>= k = Arr2T $
    \a b -> do
      x <- f a b
      getArr2T (k x) a b
  fail s = Arr2T $ \_ _ -> fail s

instance MonadTrans (Arr2T e1 e2) where
  lift m = Arr2T $ \_ _ -> m

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \x y -> return $ f x y

a2l = Arr2T $ \e1 e2 -> [e1,e2]

ex1 = getArr2T (do {x <- a2l; y <- lift [10,20,30]; return (x+y)}) 3 4
ex2 :: Monad m => m (Char, Char, (Char, Char))
ex2 = getArr2T (
  do
    x <- asks2 const
    y <- asks2 (flip const)
    z <- asks2 (,)
    return (x,y,z)
  ) 'A' 'B'
