{-
GHCi> concatOC $ Un (Un 42)
Un 42
GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concatOC $ Bi tst1 tst2 (Un tst3)
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
-}
data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) z) = Bi x y $ concatOC z
concatOC (Bi (Un x) (Bi y1 y2 y3) z) = Bi x y1 $ concatOC (Bi (Un y2) y3 z)
concatOC (Bi (Bi x1 x2 x3) y z) = Bi x1 x2 $ concatOC (Bi x3 y z)

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
tst4 = Bi tst1 tst2 (Un tst3)
ex1 = concatOC $ Un (Un 42)
ex2 = concatOC $ Bi tst1 tst2 (Un tst3)
