{-
GHCi> sl2 = StateT $ \st -> [(st,st),(st+1,st-1)]
GHCi> runStateT (do {6 <- sl2; return ()}) 5
[((),4)]
GHCi> sm = StateT $ \st -> Just (st+1,st-1)
GHCi> runStateT (do {42 <- sm; return ()}) 5
Nothing
-}

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m = fmap snd . runStateT m 

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m = fmap fst . runStateT m

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \ s -> return (x, s)

  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'
  fail s = StateT $ \_ -> fail s

sl2 = StateT $ \st -> [(st,st),(st+1,st-1)]
sm = StateT $ \st -> Just (st+1,st-1)

ex1 = runStateT (do {6 <- sl2; return ()}) 5
ex2 = runStateT (do {42 <- sm; return ()}) 5
