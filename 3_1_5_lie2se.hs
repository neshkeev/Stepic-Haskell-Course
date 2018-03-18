{-
GHCi> toSimple = runExcept . withExcept lie2se
GHCi> xs = [1,2,3]
GHCi> toSimple $ xs !!! 42
Left (Simple {getSimple = "[index (42) is too large]"})
GHCi> toSimple $ xs !!! (-2)
Left (Simple {getSimple = "[negative index]"})
GHCi> toSimple $ xs !!! 2
Right 3
GHCi> import Data.Foldable (msum)
GHCi> toSimpleFromList = runExcept . msum . map (withExcept lie2se)
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 42]
Left (Simple {getSimple = "[negative index][index (42) is too large]"})
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 2]
Right 3
-}
import Control.Monad.Trans.Except
import Data.Monoid ((<>))
import Data.Foldable (msum)

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)

newtype SimpleError = Simple { getSimple :: String }
  deriving (Eq, Show)

instance Monoid SimpleError where
  mempty = Simple mempty
  mappend (Simple e1) (Simple e2) = Simple $ e1 <> e2

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge n) = Simple $ "[index (" ++ show n ++ ") is too large]"
lie2se x = Simple $ "[negative index]"

(!!!) :: [a] -> Int -> Except ListIndexError a
xs !!! i | i < 0 = throwE ErrNegativeIndex
           | otherwise = my_ind xs i where
  my_ind :: [a] -> Int -> Except ListIndexError a
  my_ind [] _ = throwE $ ErrIndexTooLarge i
  my_ind (x:xs) 0 = except $ Right x
  my_ind (x:xs) j = my_ind xs (pred j)

toSimple = runExcept . withExcept lie2se
xs = [1,2,3]
ex1 = toSimple $ xs !!! 42
ex2 = toSimple $ xs !!! (-2)
ex3 = toSimple $ xs !!! 2
toSimpleFromList = runExcept . msum . map (withExcept lie2se)
ex4 = toSimpleFromList [xs !!! (-2), xs !!! 42]
ex5 = toSimpleFromList [xs !!! (-2), xs !!! 2]
