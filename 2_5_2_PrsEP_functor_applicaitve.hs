{-
GHCi> runPrsEP (pure 42) 0 "ABCDEFG"
(0,Right (42,"ABCDEFG"))
GHCi> charEP c = satisfyEP (== c)
GHCi> anyEP = satisfyEP (const True)
GHCi> testP = (,) <$> anyEP <* charEP 'B' <*> anyEP
GHCi> runPrsEP testP 0 "ABCDE"
(3,Right (('A','C'),"DE"))
GHCi> parseEP testP "BCDE"
Left "pos 2: unexpected C"
GHCi> parseEP testP ""
Left "pos 1: unexpected end of input"
GHCi> parseEP testP "B"
Left "pos 2: unexpected end of input"
-}

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

instance Functor PrsEP where
--fmap :: (a -> b) -> (PrsEP a) -> (PrsEP b)
  fmap f (PrsEP p) = PrsEP g where
    g pos cs = case p pos cs of
      (p1, Left x) -> (p1, Left x)
      (p1, Right (c, tail)) -> (p1, Right ((f c), tail))

instance Applicative PrsEP where
  pure x = PrsEP $ \ p1 s -> (p1, Right (x, s))
--(<*>) :: (PrsEP (a -> b)) -> (PrsEP a) -> (PrsEP b)
  PrsEP prs1 <*> prs2 = PrsEP fun where
    fun pos1 str = case prs1 pos1 str of
      (pos2, Left x) -> (pos2, Left x)
      (pos2, Right (ff, cs)) -> (runPrsEP $ ff <$> prs2) pos2 cs

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP f where
  f pos "" = (succ pos, Left ("pos " ++ (show . succ) pos ++ ": unexpected end of input"))
  f pos (c:cs) | pr c = (succ pos, Right (c, cs))
               | otherwise = (succ pos, Left ("pos " ++ (show . succ) pos ++ ": unexpected " ++ [c]))

charEP c = satisfyEP (== c)
anyEP = satisfyEP (const True)

ex1 = runPrsEP (pure 42) 0 "ABCDEFG"
testP = (,) <$> anyEP <* charEP 'B' <*> anyEP
ex2 = runPrsEP testP 0 "ABCDE"
ex3 = parseEP testP "BCDE"
ex4 = parseEP testP ""
ex5 = parseEP testP "B"
