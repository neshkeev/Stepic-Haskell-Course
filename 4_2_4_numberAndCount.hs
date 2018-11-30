{-
GHCi> numberAndCount (Leaf ())
(Leaf 1,1)
GHCi> numberAndCount (Fork (Leaf ()) () (Leaf ()))
(Fork (Leaf 1) 2 (Leaf 3),2)
-}

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.Monoid
import Control.Applicative

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving (Show)

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)

go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go (Leaf _) = do
  n <- get
  modify succ
  lift $ tell 1
  return (Leaf n)

go (Fork l _ r) = do
  left <- go l
  n <- get
  modify succ
  right <- go r
  return $ Fork left n right

ex1 = numberAndCount (Leaf ())
ex2 = numberAndCount (Fork (Leaf ()) () (Leaf ()))
