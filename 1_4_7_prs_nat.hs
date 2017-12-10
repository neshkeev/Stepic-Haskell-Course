{-
GHCi> runPrs mult "14*3"
Just (42,"")
GHCi> runPrs mult "64*32"
Just (2048,"")
GHCi> runPrs mult "77*0"
Just (0,"")
GHCi> runPrs mult "2*77AAA"
Just (154,"AAA")
-}
import Control.Applicative
import Data.Char

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

nat :: Prs Int
nat = read <$> (many1 alldigits) where
  alldigits = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|>
              char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many p

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

char :: Char -> Prs Char
char c = Prs helper where
  helper [] = Nothing
  helper (x:xs) | x == c = Just (x, xs)
                | otherwise = Nothing

ex1 = runPrs mult "14*3"
ex2 = runPrs mult "64*32"
ex3 = runPrs mult "77*0"
ex4 = runPrs mult "2*77AAA"
