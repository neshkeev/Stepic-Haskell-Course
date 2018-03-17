{-
GHCi> runExcept (tryRead "5" :: Except ReadError Int)
Right 5
GHCi> runExcept (tryRead "5" :: Except ReadError Double)
Right 5.0
GHCi> runExcept (tryRead "5zzz" :: Except ReadError Int)
Left (NoParse "5zzz")
GHCi> runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ()))
Right (True,())
GHCi> runExcept (tryRead "" :: Except ReadError (Bool, ()))
Left EmptyInput
GHCi> runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))
Left (NoParse "wrong")
-}

import Control.Monad.Trans.Except

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = go $ reads s where
  go ((r, []):_) = pure r
  go _ = throwE $ NoParse s

ex1 = runExcept (tryRead "5" :: Except ReadError Int)
ex2 = runExcept (tryRead "5" :: Except ReadError Double)
ex3 = runExcept (tryRead "5zzz" :: Except ReadError Int)
ex4 = runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ()))
ex5 = runExcept (tryRead "" :: Except ReadError (Bool, ()))
ex6 = runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))
