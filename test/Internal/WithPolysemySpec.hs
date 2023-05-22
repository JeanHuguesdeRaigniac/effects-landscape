module Internal.WithPolysemySpec
  ( module Internal.WithPolysemy,
    -- shoehorning
    normalize,
    -- interpreters
    run,
    runLogWriter,
    runVariableWriter,
  )
where

import Commons (normalize4r)
import Internal.WithPolysemy hiding (normalize, run)
import qualified Polysemy (run)
import Types
import Prelude hiding (log)

-- shoehorning

normalize :: (d, (c, (b, a))) -> (a, b, c, d)
normalize = normalize4r

-- interpreters

run :: Sem '[] a -> a
run = Polysemy.run

runLogWriter :: Sem (Log : r) a -> Sem r (Logs, a)
runLogWriter =
  runWriter @Logs
    . reinterpret \case
      Log s -> tell [s]

runVariableWriter :: Sem (Writer Variables ': r) a -> Sem r (Variables, a)
runVariableWriter = runWriter @Variables
