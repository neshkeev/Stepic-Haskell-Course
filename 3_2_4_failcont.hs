{-
GHCi> evalFailCont $ addInts "15" "12"
Right 27
GHCi> runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show)
Oops: EmptyInput
-}

{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Control.Monad(ap)
import Control.Applicative(liftA)

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }
--  deriving Functor

data ReadError = EmptyInput | NoParse String
  deriving Show

instance Functor (FailCont r e) where
  fmap = liftA

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

bindCont :: ((a -> r) -> r)
     -> (a -> (b -> r) -> r)
     -> (b -> r) -> r
bindCont f g = \h -> f (\a -> g a h)

bindFailCont :: ((a -> r) -> (e -> r) -> r)
     -> (a -> (b -> r) -> (e -> r) -> r)
     -> (b -> r) -> (e -> r) -> r
bindFailCont f g = \h -> \k -> f (\a -> g a h k) k

instance Monad (FailCont r e) where
  return x = FailCont $ \c _ -> c x
  FailCont f >>= g =
    FailCont $ \h -> \k ->
      f (\a -> runFailCont (g a) h k) k

toFailCont :: Except e a -> FailCont r e a
toFailCont e = FailCont $ \f -> \g ->
  case (runExcept e) of
    Right x -> f x
    Left x -> g x

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont f) = f Right Left

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = go $ reads s where
  go ((r, []):_) = pure r
  go _ = throwE $ NoParse s

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

ex1 = evalFailCont $ addInts "15" "12"
ex2 = runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show)

