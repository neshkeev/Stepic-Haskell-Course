newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f (Prs parser) = Prs helper where
    helper = fmap (fmap updateFirst) parser
    updateFirst (x1, x2) = (f x1, x2)

anyChr :: Prs Char
anyChr = Prs $ \s -> helper s
  where
    helper "" = Nothing
    helper (x:xs) = Just(x, xs)
