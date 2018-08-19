{-
GHCi> runIdentity (runLoggT logTst)
Logged "AAABBB" 42
GHCi> runLoggT $ failTst [5,5]
[Logged "A" 42,Logged "A" 42]
GHCi> runLoggT $ failTst [5,6]
[Logged "A" 42]
GHCi> runLoggT $ failTst [7,6]
[]
-}

import Data.Functor.Identity
import Control.Monad (ap, liftM)

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (LoggT m) where
  return x = LoggT $ return $ Logged mempty x
  fail = LoggT . fail
  (LoggT m) >>= k = LoggT $ do
    (Logged w a) <- m
    (Logged w' b) <- runLoggT $ k a
    return $ Logged (w `mappend` w') b

logTst :: LoggT Identity Integer
logTst = do 
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z
  
failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

ex1 = runIdentity (runLoggT logTst)
ex2 = runLoggT $ failTst [5,5]
ex3 = runLoggT $ failTst [5,6]
ex4 = runLoggT $ failTst [7,6]
