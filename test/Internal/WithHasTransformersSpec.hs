module Internal.WithHasTransformersSpec
  ( module Internal.WithHasTransformers,
    -- shoehorning
    C,
    CS,
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

import Commons (Constrain, normalize4l)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (Text)
import Internal.WithHasTransformers hiding (C, CS, HasLog, Log, log, normalize, run)
import Types
import Prelude hiding (log)

-- shoehorning

type C e sig es = (Monad es, Reify e es)

type CS es sig m = Constrain (Monad m ': es)

type family Reify e es where
  Reify Log m = HasLog m
  Reify (Error t) m = HasExcept t m
  Reify (Reader t) m = HasReader t m
  Reify (State t) m = HasState t m
  Reify (Writer t) m = HasWriter t m

normalize :: (((a, b), c), d) -> (a, b, c, d)
normalize = normalize4l

-- effect

data Log

type HasLog m = HasWriter Logs m

log :: (HasLog m) => Text -> m ()
log s = tell [s]

-- interpreters

run :: (Identity ~ m) => m a -> a
run = runIdentity

runLogWriter :: WriterT Logs m a -> m (a, Logs)
runLogWriter = runWriterT @Logs

runVariableWriter :: WriterT Variables m a -> m (a, Variables)
runVariableWriter = runWriter @Variables
