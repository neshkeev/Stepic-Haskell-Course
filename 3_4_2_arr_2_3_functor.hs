{-
GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
GHCi> (getArr2T $ succ <$> a2l) 10 100
[11,101,111]
GHCi> a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)
GHCi> (getArr3T $ sqrt <$> a3e) 2 3 4
Right 3.0
-}

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap f = Arr2T . (fmap . fmap . fmap) f . getArr2T

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap f = Arr3T . (fmap . fmap . fmap . fmap) f . getArr3T

a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)

ex1 = (getArr2T $ succ <$> a2l) 10 100
ex2 = (getArr3T $ sqrt <$> a3e) 2 3 4
