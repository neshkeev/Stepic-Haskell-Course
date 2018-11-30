{-
GHCi> let st = StateT (\x -> Just(x + 1, x * 2))
GHCi> evalStateT st 4
Just 5
GHCi> execStateT st 4
Just 8
-}

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = fmap (fmap (fmap fst)) runStateT

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = fmap (fmap (fmap snd)) runStateT

st = StateT (\x -> Just(x + 1, x * 2))

ex1 = evalStateT st 4
ex2 = execStateT st 4
