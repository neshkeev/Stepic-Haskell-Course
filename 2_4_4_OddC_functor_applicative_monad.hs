{-
GHCi> tst1 = Bi 10 20 (Un 30)
GHCi> tst2 = Bi 1 2 (Bi 3 4 (Un 5))
GHCi> do {x <- tst1; y <- tst2; return (x + y)}
Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))
GHCi> do {x <- tst2; y <- tst1; return (x + y)}
Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))
-}

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
--fmap :: (a -> b) -> OddC a -> OddC b
  fmap f (Un x) = Un $ f x
  fmap f (Bi x y z) = Bi (f x) (f y) (f <$> z)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) z) = Bi x y $ concatOC z
concatOC (Bi (Un x) (Bi y1 y2 y3) z) = Bi x y1 $ concatOC (Bi (Un y2) y3 z)
concatOC (Bi (Bi x1 x2 x3) y z) = Bi x1 x2 $ concatOC (Bi x3 y z)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x1 x2 x3) y z = Bi x1 x2 (concat3OC x3 y z)
concat3OC (Un x) (Bi y1 y2 y3) z = Bi x y1 (concat3OC (Un y2) y3 z)
concat3OC (Un x) (Un y) z = Bi x y z

instance Applicative OddC where
--pure :: a -> OddC a
  pure = Un
--(<*>) :: (OddC (a -> b)) -> (OddC a) -> (OddC b)
  (Un f) <*> (Un v) = Un $ f v
  (Un f) <*> (Bi v1 v2 v3) = Bi (f v1) (f v2) (f <$> v3)
  (Bi f1 f2 f3) <*> (Un v) = Bi (f1 v) (f2 v) (f3 <*> (Un v))
  (Bi f1 f2 f3) <*> (Bi v1 v2 v3) =
    let
      frst = (Bi (f1 v1) (f1 v2) (f1 <$> v3))
      scnd = (Bi (f2 v1) (f2 v2) (f2 <$> v3))
      thrd = concatOC $ (Bi (f3 <*> (Un v1)) (f3 <*> (Un v2)) (Un $ f3 <*> v3))
    in concat3OC frst scnd thrd

instance Monad OddC where
--(>>=) :: (OddC a) -> (a -> OddC b) -> (OddC b)
  (Un l) >>= f = f l
  (Bi x y z) >>= f =
    let
      frst = f x
      scnd = f y
      thrd = z >>= f
    in concat3OC frst scnd thrd

tst1 = Bi 10 20 (Un 30)
tst2 = Bi 1 2 (Bi 3 4 (Un 5))
tst3 = Bi (+1) (+2) $ Un (^2)
-- let's create an infinitive data structure
tst4 = Bi 1 2 tst4

ex1 = do {x <- tst1; y <- tst2; return (x + y)}
ex2 = do {x <- tst2; y <- tst1; return (x + y)}
-- let's try to use infinitive data structures
ex3 = tst3 <*> tst4
ex4 = do {x <- tst1; y <- tst4; return (x + y)}
