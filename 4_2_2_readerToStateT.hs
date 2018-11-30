{-
GHCi> evalStateT (readerToStateT $ asks (+2)) 4
6
GHCi> runStateT  (readerToStateT $ asks (+2)) 4
(6,4)
-}

import Control.Monad.Trans.Reader

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = fmap (fmap (fmap fst)) runStateT

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = fmap (fmap (fmap snd)) runStateT

readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT r = StateT $ \s -> do
  a <- runReaderT r s
  return (a, s)

ex1 :: (Num a, Monad m) => m a
ex1 = evalStateT (readerToStateT $ asks (+2)) 4

ex2 :: (Num a, Monad m) => m (a, a)
ex2 = runStateT  (readerToStateT $ asks (+2)) 4
