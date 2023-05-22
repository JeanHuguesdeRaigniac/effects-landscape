module Internal.WithRWSTMtlSpec
  ( module Internal.WithRWSTMtl,
    -- shoehorning
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

import Control.Monad.Identity (Identity (runIdentity), IdentityT)
import Control.Monad.Trans.Accum
import Data.Text (Text)
import Internal.WithRWSTMtl hiding (Log, log, normalize, run, runReader, runWriter)
import Types
import Prelude hiding (log)

-- shoehorning

normalize :: ((a, b, c), d) -> (a, b, c, d)
normalize ((result, steps, variables), logs) = (result, steps, variables, logs)

-- effect

type Log = MonadLog

class Monad m => MonadLog m where
  log :: Text -> m ()

instance MonadLog m => MonadLog (ExceptT e m) where
  log = lift . log

instance (MonadLog m, Monoid w) => MonadLog (RWST r w s m) where
  log = lift . log

instance (MonadLog m, Logs ~ w) => MonadLog (AccumT w m) where
  log s = add [s]

instance MonadLog Identity where
  log = log

instance MonadLog m => MonadLog (IdentityT m) where
  log = lift . log

-- interpreters

run :: Identity a -> a
run = runIdentity

runLogWriter :: AccumT Logs m a -> m (a, Logs)
runLogWriter = flip runAccumT []

runVariableWriter :: a -> a
runVariableWriter = id

runReader :: r -> RWST r w s m a -> s -> m (a, s, w)
runReader env ma = runRWST ma env
