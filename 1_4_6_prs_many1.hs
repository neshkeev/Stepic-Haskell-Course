{-
> runPrs (many1 $ char 'A') "AAABCDE"
Just ("AAA","BCDE")
> runPrs (many1 $ char 'A') "BCDE"
Nothing
-}
import Control.Applicative

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f (Prs parser) = Prs helper where
    helper = fmap (fmap updateFirst) parser
    updateFirst (x1, x2) = (f x1, x2)

instance Alternative Prs where
  empty = Prs $ \_ -> Nothing
  (<|>) (Prs f) (Prs g) = Prs fun where
    fun = (<|>) <$> f <*> g

instance Applicative Prs where
  pure a = Prs fun where
    fun s = Just (a, s)
  Prs(func) <*> val = Prs fun where
    fun s = case func s of
      Nothing -> Nothing
      Just (ff, s') -> (runPrs $ ff <$> val) s'

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many p

char :: Char -> Prs Char
char c = Prs helper where
  helper [] = Nothing
  helper (x:xs) | x == c = Just (x, xs)
                | otherwise = Nothing

ex1 = runPrs (many1 $ char 'A') "AAABCDE"
ex2 = runPrs (many1 $ char 'A') "BCDE"
