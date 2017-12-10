{-
GHCi> fmap (^2) $ Cmps3 [[[1],[2,3,4],[5,6]],[],[[7,8],[9,10,11]]]
Cmps3 {getCmps3 = [[[1],[4,9,16],[25,36]],[],[[49,64],[81,100,121]]]}
-}
{-# LANGUAGE TypeOperators #-}

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
  deriving (Eq,Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  {-
  fmap :: (a -> b) -> f a -> f b = (a -> b) -> Cmps3 f g h a -> Cmps3 f g h b
  fmap m x = Cmps3 $ omega m x = Cmps3 $ fmap (fmap (fmap m)) x

  m :: (a -> b)
  x :: f (g (h a)) -> f (g (h b))

  ksi :: (a -> b) -> h a -> h b
  ksi = fmap

  phi :: (a -> b) -> g (h a) -> g (h b)
  phi m = fmap (ksi m)

  omega :: (g (h a) -> g (h b)) -> f (g (h a)) -> f (g (h b))
  omega m x = fmap (phi m) x = fmap (fmap (ksi m)) x = fmap (fmap (fmap m)) x
  -}
  fmap m (Cmps3 x) = Cmps3 $ fmap (fmap (fmap m)) x

ex1 = fmap (^2) $ Cmps3 [[[1],[2,3,4],[5,6]],[],[[7,8],[9,10,11]]]
