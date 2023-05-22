module Internal.WithRWSTMtl
  ( module Control.Monad.Except,
    module Control.Monad.RWS.Strict,
    -- shoehorning
    C,
    CS,
    ES,
    Error,
    Reader,
    State,
    Writer,
    normalize,
    show,
    -- effect
    Log,
    log,
    -- interpreters
    run,
    runLog,
    runWriter,
    runState,
    runReader,
    runError,
  )
where

import Commons (Constrain, show)
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Data.Text (Text, unpack)
import Prelude hiding (log, show)

-- shoehorning

type C e sig es = (Monad es, e es)

type CS es sig m = (Monad m, Constrain es)

type ES m a = m a

type Error = MonadError

type Reader = MonadReader

type State = MonadState

type Writer = MonadWriter

normalize :: a -> a
normalize = id

-- effect

type Log = MonadLog

class MonadIO m => MonadLog m where
  log :: Text -> m ()

instance MonadLog m => MonadLog (ExceptT e m) where
  log = lift . log

instance (MonadLog m, Monoid w) => MonadLog (RWST r w s m) where
  log = lift . log

instance MonadLog IO where
  log = putStrLn . unpack

-- interpreters

run :: a -> a
run = id

runLog :: a -> a
runLog = id

runWriter :: a -> a
runWriter = id

runState :: s -> (s -> a) -> a
runState s f = f s

runReader :: r -> RWST r w s m a -> s -> m (a, s, w)
runReader env ma = runRWST ma env

runError :: ExceptT e m a -> m (Either e a)
runError = runExceptT
