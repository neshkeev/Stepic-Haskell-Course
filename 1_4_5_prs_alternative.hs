{-
GHCi> runPrs (char 'A' <|> char 'B') "ABC"
Just ('A',"BC")
GHCi> runPrs (char 'A' <|> char 'B') "BCD"
Just ('B',"CD")
GHCi> runPrs (char 'A' <|> char 'B') "CDE"
Nothing

-}
import Control.Applicative

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Alternative Prs where
  empty = Prs $ \_ -> Nothing
  (<|>) (Prs f) (Prs g) = Prs fun where
--  fun = fmap (<|>) (s -> Maybe (a, s)) <*> (s -> Maybe (a, s)) = {- apply fmap and expand (<*>) -}
--        \c -> ((\s -> (<|>) (Maybe (a, s))) c) ((s -> Maybe (a, s)) c) = {- beta reduction: s -> c -}
--        \c -> ((<|>) Maybe(a, c)) (Maybe (a, c)) = {- use (<|>) in the infix notation -}
--        \c -> Maybe(a, c) <|> Maybe (a, c)
    fun = (<|>) <$> f <*> g

{-
instance Functor (->) e where
  fmap :: (a -> b) -> (e -> a) -> (e -> b)
  fmap f g = \e -> f (g e)

instance Applicative (->) e where
  pure x = \_ -> x
  (<*>) :: (e -> a -> b) -> (e -> a) -> (e -> b)
  (<*>) f g = \e -> f e (g e)
-}

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

char :: Char -> Prs Char
char c = Prs helper where
  helper [] = Nothing
  helper (x:xs) | x == c = Just (x, xs)
                | otherwise = Nothing

ex1 = runPrs (char 'A' <|> char 'B') "ABC"
ex2 = runPrs (char 'A' <|> char 'B') "BCD"
ex3 = runPrs (char 'A' <|> char 'B') "CDE"
