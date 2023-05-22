module Internal.WithRWSTTransformersSpec
  ( module Internal.WithRWSTTransformers,
    -- shoehorning
    C,
    CS,
    ES,
    throwError,
    normalize,
    -- effect
    Log,
    log,
    -- interpreters
    run,
    runLogWriter,
    runVariableWriter,
    runReader,
  )
where

import Commons (Constrain)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Trans.Accum
import Data.Kind (Constraint)
import Internal.WithRWSTTransformers hiding (C, CS, ES, Log, log, normalize, run, runReader, runWriter, throwError)
import Types
import Prelude hiding (log)

-- shoehorning

type C e p es = () :: Constraint

type CS es sig r = Constrain es

type ES es a = Eval a

type Eval a = ExceptT String (RWST Env Variables Steps (AccumT Logs Identity)) a

throwError :: Monad m => e -> ExceptT e m a
throwError = throwE

normalize :: ((a, b, c), d) -> (a, b, c, d)
normalize ((result, steps, variables), logs) = (result, steps, variables, logs)

-- effect

data Log

log ::
  ( MonadTrans t1,
    MonadTrans t2,
    Monad m,
    Monad (t2 (AccumT [a] m))
  ) =>
  a ->
  t1 (t2 (AccumT [a] m)) ()
log s = lift $ lift $ add [s]

-- interpreters

run :: Identity a -> a
run = runIdentity

runLogWriter :: AccumT Logs m a -> m (a, Logs)
runLogWriter = flip runAccumT []

runVariableWriter :: a -> a
runVariableWriter = id

runReader :: r -> RWST r w s m a -> s -> m (a, s, w)
runReader env ma = runRWST ma env
