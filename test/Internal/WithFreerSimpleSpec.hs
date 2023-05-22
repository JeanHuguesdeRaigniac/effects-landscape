module Internal.WithFreerSimpleSpec
  ( module Internal.WithFreerSimple,
    -- shoehorning
    normalize,
    -- interpreters
    run,
    runLogWriter,
    runVariableWriter,
  )
where

import Commons (normalize4l)
import Control.Monad.Freer hiding (run)
import qualified Control.Monad.Freer as Freer (run)
import Internal.WithFreerSimple hiding (normalize, run)
import Types
import Prelude hiding (log)

-- shoehorning

normalize :: (((a, b), c), d) -> (a, b, c, d)
normalize = normalize4l

-- interpreters

run :: Eff '[] a -> a
run = Freer.run

runLogWriter :: Eff (Log ': effs) a -> Eff effs (a, Logs)
runLogWriter =
  runWriter @Logs
    . reinterpret \case
      Log s -> tell [s]

runVariableWriter :: Eff (Writer Variables : es) a -> Eff es (a, Variables)
runVariableWriter = runWriter @Variables
