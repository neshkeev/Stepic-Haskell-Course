{-
GHCi> runRiiEsSiT (forever $ go tickCollatz') (1,200) 27
82
41
124
62
31
94
47
142
71
214
(Left "Upper bound",214)
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
            -> (Integer,Integer)  
            -> Integer 
            -> m (Either String a, Integer)
runRiiEsSiT r p = runStateT $ runExceptT ((runReaderT r) p)

go :: Monad m => StateT Integer m Integer
   -> RiiEsSiT m ()
go st = do
  (a, b) <- ask
  lift $ lift st
  r <- lift (lift get)
  when (r <= a) (lift $ throwE "Lower bound")
  when (r >= b) (lift $ throwE "Upper bound")
