module Internal.WithMtl
  ( module Control.Monad.Except,
    module Control.Monad.Reader,
    module Control.Monad.State,
    module Control.Monad.Writer,
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

import Commons (Constrain, normalize3l, show)
import Control.Monad.Except
import Control.Monad.Reader hiding (Reader, runReader)
import Control.Monad.State hiding (State, runState)
import Control.Monad.Writer hiding (Writer, runWriter)
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

-- effect

type Log = MonadLog

class MonadIO m => MonadLog m where
  log :: Text -> m ()

instance MonadLog m => MonadLog (ExceptT e m) where
  log = lift . log

instance MonadLog m => MonadLog (ReaderT r m) where
  log = lift . log

instance MonadLog m => MonadLog (StateT s m) where
  log = lift . log

instance (MonadLog m, Monoid w) => MonadLog (WriterT w m) where
  log = lift . log

instance MonadLog IO where
  log = putStrLn . unpack

-- interpreters

run :: a -> a
run = id

runLog :: a -> a
runLog = id

runError :: ExceptT e m a -> m (Either e a)
runError = runExceptT

runReader :: r -> ReaderT r m a -> m a
runReader = flip runReaderT

runState :: b -> StateT b m a -> m (a, b)
runState = flip runStateT

runWriter :: WriterT w m a -> m (a, w)
runWriter = runWriterT

normalize :: ((a, b), c) -> (a, b, c)
normalize = normalize3l
