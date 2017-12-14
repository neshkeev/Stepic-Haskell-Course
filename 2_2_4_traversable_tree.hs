{-
GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 3 Nil)
Right (Branch (Branch Nil 1 Nil) 3 Nil)
GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 2 Nil)
Left 2
GHCi> sequenceA $ Branch (Branch Nil [1,2] Nil) [3] Nil
[Branch (Branch Nil 1 Nil) 3 Nil,Branch (Branch Nil 2 Nil) 3 Nil]
-}

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini (Branch l v r) = foldr f (f v (foldr f ini r)) l

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l v r) = Branch (fmap f l) (f v) (fmap f r)

instance Traversable Tree where
  traverse f Nil = pure Nil
  traverse f (Branch l v r) = pure Branch <*> traverse f l <*> f v <*> traverse f r

ex1 = traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 3 Nil)
ex2 = traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 2 Nil)
ex3 = sequenceA $ Branch (Branch Nil [1,2] Nil) [3] Nil
