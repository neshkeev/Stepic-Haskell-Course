{-
GHCi> maximum $ Cmps [Nothing, Just 2, Just 3]
3
GHCi> length $ Cmps [[1,2], [], [3,4,5,6,7]]
7
-}
{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldMap f (Cmps x) = foldMap (foldMap f) x

in1 = Cmps [Nothing, Just 2, Just 3]
in2 = Cmps [[1,2], [], [3,4,5,6,7]]

ex1 = maximum $ in1
ex2 = length $ in2
