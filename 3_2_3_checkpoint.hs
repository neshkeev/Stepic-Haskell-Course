{-
GHCi> runCheckpointed (< 100) $ addTens 1
31
GHCi> runCheckpointed  (< 30) $ addTens 1
21
GHCi> runCheckpointed  (< 20) $ addTens 1
11
GHCi> runCheckpointed  (< 10) $ addTens 1
1
-}

import Control.Monad.Trans.Cont

type Checkpointed a = (a -> Cont a a) -> Cont a a
runCheckpointed :: (a -> Bool)
    -> Checkpointed a
    -> a
runCheckpointed pred f = ($ cont . eval_cont) (runCont . f) id
  where
    eval_cont y c | pred $ c y = c y
                  | otherwise = y


addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

ex1 = runCheckpointed (< 100) $ addTens 1
ex2 = runCheckpointed  (< 30) $ addTens 1
ex3 = runCheckpointed  (< 20) $ addTens 1
ex4 = runCheckpointed  (< 10) $ addTens 1
