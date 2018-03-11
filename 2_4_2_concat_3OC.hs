{-
GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concat3OC tst1 tst2 tst3
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
-}
data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) (Un z) = Bi x y (Un z)
concat3OC (Un x) (Un y) z@(Bi z1 z2 z3) = Bi x y (concat3OC (Un z1) (Un z2) z3)
concat3OC (Un x) (Bi y1 y2 y3) (Un z) = Bi x y1 (concat3OC (Un y2) y3 (Un z))
concat3OC (Un x) (Bi y1 y2 y3) z@(Bi z1 z2 z3) = Bi x y1 (concat3OC (Un y2) y3 z)
concat3OC (Bi x1 x2 x3) (Un y) (Un z) = Bi x1 x2 (concat3OC x3 (Un y) (Un z))
concat3OC (Bi x1 x2 x3) (Un y) z@(Bi z1 z2 z3) = Bi x1 x2 (concat3OC x3 (Un y) z)
concat3OC (Bi x1 x2 x3) y@(Bi y1 y2 y3) (Un z) = Bi x1 x2 (concat3OC x3 y (Un z))
concat3OC (Bi x1 x2 x3) y@(Bi y1 y2 y3) z@(Bi z1 z2 z3) = Bi x1 x2 (concat3OC x3 y z)

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
ex = concat3OC tst1 tst2 tst3
