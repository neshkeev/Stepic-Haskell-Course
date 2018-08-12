{-
GHCi> (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
(([3,4,5,6,7],[0,1,2]),[8,9,10])
-}

import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate f g xs = do
  tell . filter f $ xs
  lift . tell . filter g $ xs
  return $ filter (\x -> not $ f x || g x) xs

ex = (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
