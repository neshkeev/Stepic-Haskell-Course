
import Control.Monad.Trans.Cont

showCont :: Show a => Cont String a -> String
showCont = ($ show) . runCont
