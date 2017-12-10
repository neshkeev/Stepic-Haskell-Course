newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f (Prs parser) = Prs helper where
    helper = fmap (fmap updateFirst) parser
    updateFirst (x1, x2) = (f x1, x2)

instance Applicative Prs where
  pure a = Prs fun where
    fun s = Just (a, s)
  Prs(func) <*> val = Prs fun where
    fun s = case func s of
      Nothing -> Nothing
      Just (ff, s') -> (runPrs $ ff <$> val) s'

anyChr :: Prs Char
anyChr = Prs $ \s -> helper s
  where
    helper "" = Nothing
    helper (x:xs) = Just(x, xs)
