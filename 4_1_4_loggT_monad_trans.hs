{-
GHCi> runState (runLoggT logSt) 2
(Logged "30" 300,42)
-}
import Data.Functor.Identity
import Control.Monad (ap, liftM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

type Logg = LoggT Identity

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

instance MonadTrans LoggT where
--  lift = LoggT . ((Logged mempty) <$>)
  lift m = LoggT $ do
    a <- m
    return $ Logged mempty a

logSt :: LoggT (State Integer) Integer
logSt = do 
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100


write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

ex1 = runState (runLoggT logSt) 2
