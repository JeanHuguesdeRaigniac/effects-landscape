module Internal.WithExtensibleEffectsSpec
  ( module Internal.WithExtensibleEffects,
    -- shoehorning
    normalize,
    -- interpreters
    run,
    runLogWriter,
    runVariableWriter,
  )
where

import Commons (normalize4l)
import Control.Eff (run)
import Control.Eff.Operational
import Internal.WithExtensibleEffects hiding (normalize, run, runWriter)
import Types
import Prelude hiding (log)

-- shoehorning

normalize :: (((a, b), c), d) -> (a, b, c, d)
normalize = normalize4l

-- interpreters

runLogWriter :: Eff (Program Log' : Writer Logs : r) a -> Eff r (a, Logs)
runLogWriter =
  runMonoidWriter @Logs
    . runProgram \case
      Log a -> tell [a]

runVariableWriter :: Eff (Writer Variables : es) a -> Eff es (a, Variables)
runVariableWriter = runWriter @Variables

runWriter :: Monoid w => Eff (Writer w : r) a -> Eff r (a, w)
runWriter = runMonoidWriter
