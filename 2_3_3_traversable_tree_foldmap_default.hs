{-
GHCi> testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

      4
     / \
    2   5
   / \
  1   3

GHCi> foldMapDefault (\x -> [x]) testTree
[1,3,2,5,4]
-}

import Data.Traversable (foldMapDefault, fmapDefault)

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse f Nil = pure Nil
  traverse f (Branch l v r) = pure (flip . Branch) <*> traverse f l <*> traverse f r <*> f v

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
ex1 = foldMapDefault (\x -> [x]) testTree
