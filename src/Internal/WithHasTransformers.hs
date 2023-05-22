module Internal.WithHasTransformers
  ( module Control.Monad.Trans.Has,
    module Control.Monad.Trans.Has.Except,
    module Control.Monad.Trans.Has.Reader,
    module Control.Monad.Trans.Has.State,
    module Control.Monad.Trans.Has.Writer,
    -- shoehorning
    C,
    CS,
    ES,
    Error,
    Reader,
    State,
    Writer,
    throwError,
    local,
    normalize,
    show,
    -- effect
    Log,
    HasLog,
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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (runIdentityT))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Has
import Control.Monad.Trans.Has.Except
import Control.Monad.Trans.Has.Reader
import Control.Monad.Trans.Has.State
import Control.Monad.Trans.Has.Writer
import Data.Text (Text, unpack)
import Prelude hiding (log, show)

-- shoehorning

type C e sig es = (Monad es, Reify e es)

type family Reify e es where
  Reify Log m = HasLog m
  Reify (Error t) m = HasExcept t m
  Reify (Reader t) m = HasReader t m
  Reify (State t) m = HasState t m
  Reify (Writer t) m = HasWriter t m

type CS es sig m = Constrain (Monad m ': es)

type ES m a = m a

data Error t

data Reader t

data State t

data Writer t

throwError :: HasExcept String m => String -> m a
throwError e = liftH $ ExceptT $ return $ Left e

local :: (Monad m, HasReader r m) => (r -> r) -> ReaderT r m b -> m b
local f tm = do
  env <- ask
  runReader (f env) tm

normalize :: ((a, b), c) -> (a, b, c)
normalize = normalize3l

-- effect

data Log

type HasLog m = (MonadIO m, Has IdentityT m)

log :: HasLog m => Text -> m ()
log = liftIO . putStrLn . unpack

-- interpreters

run :: a -> a
run = id

runLog :: IdentityT f a -> f a
runLog = runIdentityT

runWriter :: WriterT w m a -> m (a, w)
runWriter = runWriterT

runState :: b -> StateT b m a -> m (a, b)
runState = flip runStateT

runReader :: r -> ReaderT r m a -> m a
runReader = flip runReaderT

runError :: ExceptT e m a -> m (Either e a)
runError = runExceptT
