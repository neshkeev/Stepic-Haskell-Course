{-
GHCi> pure 42 :: ([] |.| [] |.| []) Int
Cmps {getCmps = [Cmps {getCmps = [[42]]}]}
GHCi> unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
[[[42]]]
GHCi> unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int)
[Just [42]]
GHCi> unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
[[[[42]]]]
-}
{-# LANGUAGE TypeOperators #-}
import Control.Applicative

infixr 9 |.|

newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap m (Cmps x) = Cmps $ fmap (fmap m) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  x <*> y = undefined


unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap unCmps3 . getCmps

ex1 = pure 42 :: ([] |.| [] |.| []) Int
ex2 = unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
ex3 = unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int)
ex4 = unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
