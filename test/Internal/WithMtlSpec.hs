module Internal.WithMtlSpec
  ( module Internal.WithMtl,
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

import Commons (normalize4l)
import Control.Monad.Identity (Identity (runIdentity), IdentityT)
import Control.Monad.Trans.Accum
import Data.Text (Text)
import Internal.WithMtl hiding (Log, log, normalize, run)
import Types
import Prelude hiding (log)

-- shoehorning

normalize :: (((a, b), c), d) -> (a, b, c, d)
normalize = normalize4l

-- effect

type Log = MonadLog

class Monad m => MonadLog m where
  log :: Text -> m ()

instance MonadLog m => MonadLog (ExceptT e m) where
  log = lift . log

instance MonadLog m => MonadLog (ReaderT r m) where
  log = lift . log

instance MonadLog m => MonadLog (StateT s m) where
  log = lift . log

instance (MonadLog m, Monoid w) => MonadLog (WriterT w m) where
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

runVariableWriter :: WriterT Variables m a -> m (a, Variables)
runVariableWriter = runWriter @Variables
