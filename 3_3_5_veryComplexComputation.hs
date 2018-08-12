{-
GHCi> runMyRWT veryComplexComputation ["abc","defg","hij"]
Nothing
GHCi> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
Just (("KL","HIJ"),"defg,abc")
-}

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Char (toUpper)

type MyRWT m = ReaderT [String] (WriterT String m)

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  evens <- myAsks (filter (even . length))
  odds <- myAsks (filter (odd . length))
  case (evens, odds) of
    ((e1 : e2 : _), (o1 : o2 : _)) -> myTell e1 >> myTell "," >> myTell o1 >> return (map toUpper e2, map toUpper o2)
    _ -> myLift Nothing

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rw s = runWriterT $ runReaderT rw s

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

ex1 = runMyRWT veryComplexComputation ["abc","defg","hij"]
ex2 = runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
