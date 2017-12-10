{-# LANGUAGE TypeOperators #-}

infixr 9 |.|

newtype (|.|) f g a = Cmps { getCmps :: f (g a) }
type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (1, ('x', True))

b :: B t
b = Cmps (True, id, Right 5)

c :: C
c  = Cmps $ \x y -> y
