{-
GHCi> runExcept $ trySum ["10", "20", "30"]
Right 60
GHCi> runExcept $ trySum ["10", "20", ""]
Left (SumError 3 EmptyInput)
GHCi> runExcept $ trySum ["10", "two", "30"]
Left (SumError 2 (NoParse "two"))
-}

import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Monad.Trans.Except

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = go $ reads s where
  go ((r, ""):_) = pure r
  go _ = throwE $ NoParse s

data SumError = SumError Int ReadError
  deriving Show

trySum :: [String] -> Except SumError Integer
trySum xx = fmap (getSum . fold) $ traverse (fmap Sum) $ zipWith (withExcept . SumError) [1..] $ fmap tryRead xx

ex1 = runExcept $ trySum ["10", "20", "30"]
ex2 = runExcept $ trySum ["10", "20", ""]
ex3 = runExcept $ trySum ["10", "two", "30"]
