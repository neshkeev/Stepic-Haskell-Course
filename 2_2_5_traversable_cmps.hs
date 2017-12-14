{-
GHCi> sequenceA (Cmps [Just (Right 2), Nothing])
Right (Cmps {getCmps = [Just 2,Nothing]})
GHCi> sequenceA (Cmps [Just (Left 2), Nothing])
Left 2
-}

{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap f (Cmps x) = Cmps $ fmap (fmap f) x

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldMap f (Cmps x) = foldMap (foldMap f) x

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse f (Cmps x) = pure Cmps <*> traverse (traverse f) x

ex1 = sequenceA (Cmps [Just (Right 2), Nothing])
ex2 = sequenceA (Cmps [Just (Left 2), Nothing])
