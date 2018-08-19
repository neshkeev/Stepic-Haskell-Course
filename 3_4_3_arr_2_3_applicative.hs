{-
GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> a2fl = Arr2T $ \e1 e2 -> [(e1*e2+),const 7]
GHCi> getArr2T (a2fl <*> a2l) 2 10
[22,30,7,7]
GHCi> a3fl = Arr3T $ \e1 e2 e3 -> [(e2+),(e3+)]
GHCi> a3l = Arr3T $ \e1 e2 e3 -> [e1,e2]
GHCi> getArr3T (a3fl <*> a3l) 3 5 7
[8,10,10,12]
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

a2l = Arr2T $ \e1 e2 -> [e1,e2]
a2fl = Arr2T $ \e1 e2 -> [(e1*e2+),const 7]

a3fl = Arr3T $ \e1 e2 e3 -> [(e2+),(e3+)]
a3l = Arr3T $ \e1 e2 e3 -> [e1,e2]

ex1 = getArr2T (a2fl <*> a2l) 2 10
ex2 = getArr3T (a3fl <*> a3l) 3 5 7
{-

instance Monad (Arr2T e1 e2 m) where
  (>>=) = undefined

instance Monad (Arr3T e1 e2 e3 m) where
  (>>=) = undefined
-}



