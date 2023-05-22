module Internal.WithFusedEffectsSpec
  ( module Internal.WithFusedEffects,
    -- shoehorning
    normalize,
    -- effect
    Log,
    log,
    -- interpreters
    run,
    runLogWriter,
    runVariableWriter,
  )
where

import Commons (normalize4r)
import Control.Monad.Identity
import Data.Text (Text)
import Internal.WithFusedEffects hiding (Log, log, normalize, run)
import Types
import Prelude hiding (log)

-- shoehorning

normalize :: (d, (c, (b, a))) -> (a, b, c, d)
normalize = normalize4r

-- effect

type Log = Writer Logs

log :: Has Log sig m => Text -> m ()
log s = tell [s]

-- interpreters

run :: Identity a -> a
run = runIdentity

runLogWriter :: WriterC Logs m a -> m (Logs, a)
runLogWriter = runWriter @Logs

runVariableWriter :: WriterC Variables m a -> m (Variables, a)
runVariableWriter = runWriter @Variables
