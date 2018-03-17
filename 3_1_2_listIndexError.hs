{-
GHCi> runExcept $ [1..100] !!! 5
Right 6
GHCi> (!!!!) xs n = runExcept $ xs !!! n
GHCi> [1,2,3] !!!! 0
Right 1
GHCi> [1,2,3] !!!! 42
Left (ErrIndexTooLarge 42)
GHCi> [1,2,3] !!!! (-3)
Left ErrNegativeIndex
-}
import Control.Monad.Trans.Except

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)


(!!!) :: [a] -> Int -> Except ListIndexError a
xs !!! i | i < 0 = throwE ErrNegativeIndex
           | otherwise = my_ind xs i where
  my_ind :: [a] -> Int -> Except ListIndexError a
  my_ind [] _ = throwE $ ErrIndexTooLarge i
  my_ind (x:xs) 0 = except $ Right x
  my_ind (x:xs) j = my_ind xs (pred j)

ex1 = runExcept $ [1..100] !!! 5
(!!!!) xs n = runExcept $ xs !!! n
ex2 = [1,2,3] !!!! 0
ex3 = [1,2,3] !!!! 42
ex4 = [1,2,3] !!!! (-3)
