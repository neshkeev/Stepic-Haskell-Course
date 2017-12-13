{-
GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
Right (Tr 12 14 16)
GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
Left 8
GHCi> sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)
-}

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f g h) <*> (Tr a b c) = Tr (f a) (g b) (h c)

instance Foldable Triple where
  foldr f ini (Tr x y z) = x `f` (y `f` (z `f` ini))

instance Traversable Triple where
  traverse f (Tr a b c) = Tr <$> f a <*> f b <*> f c

ex1 = foldl (++) "!!" (Tr "ab" "cd" "efg")
ex2 = traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
ex3 = traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
ex4 = sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
