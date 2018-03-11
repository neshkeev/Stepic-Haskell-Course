{-
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC"
Right (('A','B'),"C")
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD"
Left "unexpected C"
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD"
Left "unexpected B"
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

instance Monad PrsE where
  (PrsE p) >>= f = PrsE go where
    go s = case p s of
      Left x -> Left x
      Right (x, tail) -> (runPrsE $ f x) tail

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
  f "" = Left "unexpected end of input"
  f (c:cs) | pr c = Right (c, cs)
           | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

ex1 = runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC"
ex2 = runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD"
ex3 = runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD"
