{-
GHCi> decode one hundred twenty three as a number
123
GHCi> decode one hundred twenty one as a number
121
GHCi> decode one hundred twenty as a number
120
GHCi> decode one hundred as a number
100
GHCi> decode three hundred as a number
300
GHCi> decode two thousand seventeen as a number
2017
-}
decode :: (Int -> r) -> r
decode c = c 0

as :: Int -> (Int -> r) -> r
as i c = c i

a :: Int -> (Int -> r) -> r
a i c = c i

number :: Int -> Int
number = id

one :: Int -> (Int -> r) -> r
one i c = c $ i + 1

two :: Int -> (Int -> r) -> r
two i c = c $ i + 2

three :: Int -> (Int -> r) -> r
three i c = c $ i + 3

seventeen :: Int -> (Int -> r) -> r
seventeen i c = c $ i + 17

twenty :: Int -> (Int -> r) -> r
twenty i c = c $ i + 20

hundred :: Int -> (Int -> r) -> r
hundred i c = c $ i * 100

thousand :: Int -> (Int -> r) -> r
thousand i c = c $ i * 1000

ex1 = decode one hundred twenty three as a number
ex2 = decode one hundred twenty one as a number
ex3 = decode one hundred twenty as a number
ex4 = decode one hundred as a number
ex5 = decode three hundred as a number
ex6 = decode two thousand seventeen as a number
