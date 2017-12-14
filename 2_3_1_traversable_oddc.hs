{-
GHCi> cnt1 = Un 42
GHCi> cnt3 = Bi 1 2 cnt1
GHCi> cnt5 = Bi 3 4 cnt3
GHCi> cnt5
Bi 3 4 (Bi 1 2 (Un 42))
GHCi> cntInf = Bi 'A' 'B' cntInf
GHCi> cntInf
Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'Interrupted.
GHCi>

GHCi> (+1) <$> cnt5
Bi 4 5 (Bi 2 3 (Un 43))
GHCi> toList cnt5
[3,4,1,2,42]
GHCi> sum cnt5
52
GHCi> traverse (\x->[x+2,x-2]) cnt1
[Un 44,Un 40]
-}
import Data.Foldable (toList)

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
  fmap f (Un x) = Un $ f x
  fmap f (Bi x y z) = Bi (f x) (f y) (fmap f z)

instance Foldable OddC where
  foldr f ini (Un x) = f x ini
  foldr f ini (Bi x y z) = x `f` (y `f` (foldr f ini z))

instance Traversable OddC where
  traverse f (Un x) = pure Un <*> f x
  traverse f (Bi x y z) = pure Bi <*> f x <*> f y <*> (traverse f z)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3

ex1 = (+1) <$> cnt5
ex2 = toList cnt5
ex3 = sum cnt5
ex4 = traverse (\x->[x+2,x-2]) cnt1
