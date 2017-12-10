{-
GHCi> let anyE = satisfyE (const True)
GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
Right (('A','C'),"DE")
GHCi> runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
Left "unexpected B"
GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
Left "unexpected end of input"
-}

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
  fmap f (PrsE parser) = PrsE fun where
    fun = fmap (fmap updateFirst) parser
    updateFirst (x, y) = (f x, y)

instance Applicative PrsE where
  pure x = PrsE f where
    f s = Right (x, s)
  (<*>) (PrsE fs) vs = PrsE f where
    f s = case fs s of
      Left x -> Left x
      Right (func, y) -> (runPrsE $ func <$> vs) y

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
  f "" = Left "unexpected end of input"
  f (c:cs) | pr c = Right (c, cs)
           | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
