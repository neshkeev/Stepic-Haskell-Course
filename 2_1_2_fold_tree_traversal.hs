{-
GHCi> tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
GHCi> foldr (:) [] tree
[1,2,3,4]
GHCi> foldr (:) [] $ PreO tree
[3,1,2,4]
GHCi> foldr (:) [] $ PostO tree
[2,1,4,3]
GHCi> foldr (:) [] $ LevelO tree
[3,1,4,2]
-}

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

{-
       3
     /  \
    1    4
     \
      2

-}
tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

-- correct (in-order)
{-
f = (:)
ini = []

in-order:
foldr (:) [] tree = foldr (:) ((:) 3 (foldr (:) [] (Branch Nil 4 Nil))) (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) ((:) 3 (foldr (:) ((:) 4 (foldr (:) [] Nil)) Nil)) (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) ((:) 3 (foldr (:) ((:) 4 []) Nil)) (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) ((:) 3 [4]) (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) [3,4] (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) ((:) 1 (foldr (:) [3,4] (Branch Nil 2 Nil))) Nil =
     foldr (:) ((:) 1 (foldr (:) ((:) 2 (foldr (:) [3,4] Nil)) Nil)) Nil =
     foldr (:) ((:) 1 (foldr (:) ((:) 2 [3,4]) Nil)) Nil =
     foldr (:) ((:) 1 [2, 3, 4]) Nil =
     foldr (:) [1,2,3,4] Nil =
     [1, 2, 3, 4]
-}
instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini (Branch l v r) = foldr f (f v (foldr f ini r)) l

{-
f = (:)
ini = []
pre-order:

foldr (:) [] tree = f v (foldr f (foldr f ini r) l) =
    (:) 3 (foldr (:) (foldr (:) [] (Branch Nil 4 Nil)) (Branch Nil 1 (Branch Nil 2 Nil))) =
    (:) 3 (foldr (:) ((:) 4 (foldr (:) (foldr (:) [] Nil) Nil)) (Branch Nil 1 (Branch Nil 2 Nil))) =
    (:) 3 (foldr (:) [4] (Branch Nil 1 (Branch Nil 2 Nil))) =
    (:) 3 ((:) 1 (foldr (:) (foldr (:) [4] (Branch Nil 2 Nil)) Nil)) =
    (:) 3 ((:) 1 (foldr (:) ((:) 2 (foldr (:) (foldr (:) [4] Nil) Nil)) Nil)) =
    (:) 3 ((:) 1 (foldr (:) [2,4] Nil)) =
    (:) 3 ((:) 1 [2,4]) =
    (:) 3 [1,2,4] =
    [3,1,2,4]
-}
instance Foldable Preorder where
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l v r)) = f v (foldr f (foldr f ini (PreO r)) (PreO l))

{-
f = (:)
ini = []
post-order:

foldr (:) [] (Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)) =
     foldr (:) (foldr (:) ((:) 3 []) (Branch Nil 4 Nil)) (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) (foldr (:) [3] (Branch Nil 4 Nil)) (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) (foldr (:) (foldr (:) ((:) 4 [3]) Nil) Nil)  (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) [4,3] (Branch Nil 1 (Branch Nil 2 Nil)) =
     foldr (:) (foldr (:) ((:) 1 [4,3]) (Branch Nil 2 Nil)) Nil =
     foldr (:) [1,4,3] (Branch Nil 2 Nil) =
     foldr (:) (foldr (:) ((:) 2 [1,4,3] Nil)) Nil =
     [2,1,4,3]
-}
instance Foldable Postorder where
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l v r)) = foldr f (foldr f (f v ini) (PostO r)) (PostO l)

{-
level-order:
foldr (:) [] (Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)) =
     helper [(Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil))] =
     (:) 3 (helper [(Branch Nil 1 (Branch Nil 2 Nil)), (Branch Nil 4 Nil)]) =
     (:) 3 ((:) 1 (helper [(Branch Nil 4 Nil), (Branch Nil 2 Nil)])) =
     (:) 3 ((:) 1 ((:) 4 helper [(Branch Nil 2 Nil)])) =
     (:) 3 ((:1) ((:) 4 ((:) 2 []))) =
     [3,1,4,2]
-}
instance Foldable Levelorder where
  foldr f ini (LevelO Nil) = ini
  foldr f ini (LevelO tree) = helper [tree] where
    helper [] = ini
    helper (Nil:xs) = helper xs
    helper ((Branch ln v rn):xs) = f v (helper $ xs ++ [ln, rn])

ex1 = foldr (:) [] tree
ex2 = foldr (:) [] $ PreO tree
ex3 = foldr (:) [] $ PostO tree
ex4 = foldr (:) [] $ LevelO tree
