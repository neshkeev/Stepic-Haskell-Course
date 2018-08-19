
import Data.Functor.Identity
import Data.Traversable
import Control.Monad.Trans.Writer.Lazy

newtype StrictWriter w a = StrictWriter { runStrictWriter :: (a, w) } deriving Show

instance Functor (StrictWriter w) where
  fmap f  = StrictWriter . updater . runStrictWriter
    where updater (x, log) = (f x, log)

instance Monoid w => Applicative (StrictWriter w) where
  pure x  = StrictWriter (x, mempty)
  f <*> v = StrictWriter $ updater (runStrictWriter f) (runStrictWriter v)
    where updater (g, w) (x, w') = (g x, w `mappend` w')

actionLazy :: WriterT String Identity Int
actionLazy = writer (42,"Hello!")
actionStrict = StrictWriter (42,"Hello!")

{-
`ex1` diverges due to `sequenceA` has to fold an infinite list of
`StrictWriter` and only then take 5 elements.
-}
ex1 = runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
{-
`ex2` converges, because `sequnceA` doesn't need to fold the whole list of `WriterT` due to its lazy nature. It evaluates it as it goes and takes only 5 elements
-}
ex2 = fst . runWriter $ take 5 <$> sequenceA (repeat actionLazy)
{-
`ex3` diverges, because it works exactly as `ex1` up until the final step and then takes the result of the calculation.
-}
ex3 = fst . runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
{-
`ex4` diverges. Despite the fact that it is lazy writer and `sequenceA` doesn't fold an infinite list of `WriterT`, it takes only fist 5 elements of the results, but doesn't not limit the log concatenation (as in `ex2` with `fst`), so Haskell evaluates it and concatenates the infinite list of log values.
-}
ex4 = runWriter $ take 5 <$> sequenceA (repeat actionLazy)
