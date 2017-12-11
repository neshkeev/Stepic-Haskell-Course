{-
GHCi> e1 = mkEndo [(+5),(*3),(^2)]
GHCi> appEndo e1 2
17
GHCi> e2 = mkEndo (42,(*3))
GHCi> appEndo e2 2
6
-}

import Data.Monoid

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

e1 = mkEndo [(+5),(*3),(^2)]
e2 = mkEndo (42,(*3))
ex1 = appEndo e1 2
ex2 = appEndo e2 2
