{-
GHCi> charEP c = satisfyEP (== c)
GHCi> runPrsEP (charEP 'A') 0 "ABC"
(1,Right ('A',"BC"))
> runPrsEP (charEP 'A') 41 "BCD"
(42,Left "pos 42: unexpected B")
> runPrsEP (charEP 'A') 41 ""
(42,Left "pos 42: unexpected end of input")

GHCi> parseEP (charEP 'A') "ABC"
Right ('A',"BC")
GHCi> parseEP (charEP 'A') "BCD"
Left "pos 1: unexpected B"
GHCi> parseEP (charEP 'A') ""
Left "pos 1: unexpected end of input"
-}

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP f where
  f pos "" = (succ pos, Left ("pos " ++ (show . succ) pos ++ ": unexpected end of input"))
  f pos (c:cs) | pr c = (succ pos, Right (c, cs))
               | otherwise = (succ pos, Left ("pos " ++ (show . succ) pos ++ ": unexpected " ++ [c]))

charEP c = satisfyEP (== c)
ex1 = runPrsEP (charEP 'A') 0 "ABC"
ex2 = runPrsEP (charEP 'A') 41 "BCD"
ex3 = runPrsEP (charEP 'A') 41 ""

ex4 = parseEP (charEP 'A') "ABC"
ex5 = parseEP (charEP 'A') "BCD"
ex6 = parseEP (charEP 'A') ""
