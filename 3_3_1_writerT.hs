{-
GHCi> runReader (runWriterT logFirstAndRetSecond) strings
("DEFG","abc")
-}
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans (lift)
import Data.Char (toUpper)

logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
  el1 <- lift $ asks head
  tell el1
  el2 <- lift $ asks (map toUpper . head . tail)
  return el2

strings = ["abc", "defg", "hij"]
ex = runReader (runWriterT logFirstAndRetSecond) strings
