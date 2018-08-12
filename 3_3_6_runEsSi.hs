{-
GHCi> runEsSi (go 1 85 tickCollatz) 27
(Right (),82)
GHCi> runEsSi (go 1 80 tickCollatz) 27
(Left "Upper bound",82)
GHCi> runEsSi (forever $ go 1 1000 tickCollatz) 27
(Left "Upper bound",1186)
GHCi> runEsSi (forever $ go 1 10000 tickCollatz) 27
(Left "Lower bound",1)
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Char (toUpper)

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a
        -> Integer
        -> (Either String a, Integer)
runEsSi = runState . runExceptT

go :: Integer
   -> Integer
   -> State Integer Integer
   -> EsSi ()
go a b st = do
  lift st
  r <- lift get
  when (r <= a) (throwE "Lower bound")
  when (r >= b) (throwE "Upper bound")

ex1 = runEsSi (go 1 85 tickCollatz) 27
ex2 = runEsSi (go 1 80 tickCollatz) 27
ex3 = runEsSi (forever $ go 1 1000 tickCollatz) 27
ex4 = runEsSi (forever $ go 1 10000 tickCollatz) 27
