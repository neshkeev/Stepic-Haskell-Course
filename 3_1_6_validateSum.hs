{-
GHCi> getValidate $ validateSum ["10", "20", "30"]
Right 60
GHCi> getValidate $ validateSum ["10", "", "30", "oops"]
Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]
-}
import Control.Monad.Trans.Except
import Control.Applicative
import Control.Monad
import Data.Monoid

newtype Validate e a = Validate { getValidate :: Either [e] a }
  deriving Show

data ReadError = EmptyInput | NoParse String
  deriving Show

data SumError = SumError Int ReadError
  deriving Show

instance Functor (Validate e) where
  fmap = liftA

instance Applicative (Validate e) where
  pure = Validate . Right
--(<*>) :: (Validate (Either [e] (a -> b))) -> (Validate (Either [e] a)) -> (Validate (Either [e] b))
  (Validate (Left e1)) <*> (Validate (Left e2)) = Validate $ Left $ e1 <> e2
  (Validate (Left e1)) <*> _ = Validate $ Left e1
  (Validate (Right f)) <*> (Validate x) = Validate $ f <$> x

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = go $ reads s where
  go ((r, ""):_) = pure r
  go _ = throwE $ NoParse s

collectE :: Except e a -> Validate e a
collectE = Validate . (either (Left . pure) Right) . runExcept

validateSum :: [String] -> Validate SumError Integer
validateSum xx = fmap sum $ traverse collectE $ zipWith (withExcept . SumError) [1..] $ fmap tryRead xx


ex1 = getValidate $ validateSum ["10", "20", "30"]
ex2 = getValidate $ validateSum ["10", "", "30", "oops"]
