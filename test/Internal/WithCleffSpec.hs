module Internal.WithCleffSpec
  ( module Internal.WithCleff,
    -- shoehorning
    normalize,
    -- interpreters
    run,
    runLogWriter,
    runVariableWriter,
  )
where

import Commons (normalize4l)
import Internal.WithCleff hiding (normalize, run)
import Types
import Prelude hiding (log)

-- shoehorning

normalize :: (((a, b), c), d) -> (a, b, c, d)
normalize = normalize4l

-- interpreters

run :: Eff '[] a -> a
run = runPure

runLogWriter :: Eff (Log : es) a -> Eff es (a, Logs)
runLogWriter =
  runWriter @Logs
    . reinterpret \case
      Log s -> tell [s]

runVariableWriter :: Eff (Writer Variables : es) a -> Eff es (a, Variables)
runVariableWriter = runWriter @Variables
