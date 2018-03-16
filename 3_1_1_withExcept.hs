import Control.Monad(liftM, ap)

newtype Except e a = Except { runExcept :: Either e a } deriving Show

except :: Either e a -> Except e a
except = Except

instance Functor (Except e) where
  fmap = liftM

instance Applicative (Except e) where
  pure = return
  (<*>) = ap

instance Monad (Except e) where
  return = except . Right
  (Except (Right x)) >>= f = f x
  (Except (Left x)) >>= _ = except $ Left x

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except (Left x)) = Except $ Left $ f x
withExcept _ (Except (Right x)) = Except $ Right x
