{-
GHCi> runPrsEP empty 0 "ABCDEFG"
(0,Left "pos 0: empty alternative")
GHCi> charEP c = satisfyEP (== c)
GHCi> tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE"
Left "pos 3: unexpected E"
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE"
Left "pos 3: unexpected E"
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF"
Left "pos 2: unexpected E"
-}
import Control.Applicative

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

instance Alternative PrsEP where
  empty = PrsEP $ \pos _ -> (pos, Left ("pos " ++ show pos ++ ": empty alternative"))
--(<|>) :: PrsEP a -> PrsEP a -> PrsEP a
  (PrsEP parser1) <|> (PrsEP parser2) = PrsEP parser3 where
    parser3 start str = case parser1 start str of
      (end, Right x) -> (end, Right x)
      (end1, Left x1) -> case parser2 start str of
        (end, Right x) -> (end, Right x)
        (end2, Left x2) -> mostSuccessfull (end1, Left x1) (end2, Left x2)
    mostSuccessfull l@(end1, Left x1) r@(end2, Left x2) = if x1 > x2 then l else r

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP f where
  f pos "" = (succ pos, Left ("pos " ++ (show . succ) pos ++ ": unexpected end of input"))
  f pos (c:cs) | pr c = (succ pos, Right (c, cs))
               | otherwise = (succ pos, Left ("pos " ++ (show . succ) pos ++ ": unexpected " ++ [c]))

charEP c = satisfyEP (== c)
tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

ex1 = runPrsEP empty 0 "ABCDEFG"
ex2 = parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE"
ex3 = parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE"
ex4 = parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF"
