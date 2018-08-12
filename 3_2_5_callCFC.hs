{-
*Main> evalFailCont $ addInts "15" "12"
Right 27
*Main> evalFailCont $ addInts "15" "122"
Right 42
*Main> runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show)
Oops: EmptyInput
-}

{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Control.Monad(ap, when)
import Control.Applicative(liftA)

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }
--  deriving Functor

instance Functor (FailCont r e) where
  fmap = liftA

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

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


callCC' :: (
               (a -> (b -> r) -> r)
            -> (a -> r) -> r
           )
        -> (a -> r)
        -> r
callCC' f g = f (\a -> \_ -> g a) g

callCFC :: ((a -> FailCont r e b) -> FailCont r e a)
        -> FailCont r e a
callCFC f =
  FailCont $
    \g ->
      \h ->
        runFailCont (f (\a -> FailCont $ \_ -> \_ -> g a)) g h

callCFC' :: (
               (
                  a
               -> (b -> r)
               -> (e -> r)
               -> r
               )
            -> (a -> r)
            -> (e -> r)
            -> r
            )
         -> (a -> r)
         -> (e -> r)
         -> r
callCFC' f = \g -> \h -> f (\a -> \_ -> \_ -> g a) g h

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = go $ reads s where
  go ((r, []):_) = pure r
  go _ = throwE $ NoParse s

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = callCFC $ \k -> do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  when (i1 + i2 > 100) (k 42)
  return $ i1 + i2

ex1 = evalFailCont $ addInts "15" "12"
ex2 = evalFailCont $ addInts "15" "122"
ex3 = runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show)
