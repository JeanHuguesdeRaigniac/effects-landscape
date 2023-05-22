module Internal.WithEffectfulSpec
  ( module Internal.WithEffectful,
    -- shoehorning
    normalize,
    -- interpreters
    run,
    runLogWriter,
    runVariableWriter,
  )
where

import Commons (normalize4l)
import Effectful.Dispatch.Dynamic
import Internal.WithEffectful hiding (normalize, run)
import Types
import Prelude hiding (log)

-- shoehorning

normalize :: (((a, b), c), d) -> (a, b, c, d)
normalize = normalize4l

-- interpreters

run :: Eff '[] a -> a
run = runPureEff

runLogWriter :: Eff (Log : es) a -> Eff es (a, Logs)
runLogWriter = reinterpret (runWriter @Logs) $ \_ -> \case
  Log s -> tell [s]

runVariableWriter :: Eff (Writer Variables : es) a -> Eff es (a, Variables)
runVariableWriter = runWriter @Variables
