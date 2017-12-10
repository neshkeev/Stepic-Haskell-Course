{-
GHCi> foldr (++) "!!" (Tr "ab" "cd" "efg")
"abcdefg!!"
GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
-}
data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
--foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f ini (Tr x y z) = x `f` (y `f` (z `f` ini))
--foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f ini (Tr x y z) = ((ini `f` x) `f` y) `f` z

ex1 = foldr (++) "!!" (Tr "ab" "cd" "efg")
ex2 = foldl (++) "!!" (Tr "ab" "cd" "efg")
