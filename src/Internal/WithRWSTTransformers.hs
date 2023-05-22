module Internal.WithRWSTTransformers
  ( module Control.Monad.Morph,
    module Control.Monad.Trans.Except,
    module Control.Monad.Trans.RWS.Lazy,
    -- shoehorning
    C,
    CS,
    ES,
    Error,
    Reader,
    State,
    Writer,
    ask,
    local,
    get,
    put,
    tell,
    throwError,
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

import Commons (show)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Morph
import Control.Monad.Trans.Except hiding (liftCallCC)
import Control.Monad.Trans.RWS.Lazy hiding (ask, get, local, put, tell)
import qualified Control.Monad.Trans.RWS.Lazy as RWS
import Data.Kind (Constraint)
import Data.Text (Text, unpack)
import Types (Env, Steps, Value, Variables)
import Prelude hiding (log, show)

-- shoehorning

type C e sig es = (Monad es)

type CS es sig m = (Monad m, Constrain es)

type family Constrain es :: Constraint where
  Constrain '[] = ()
  Constrain (C Log _ m ': es) = (MonadIO m, Monad m, Constrain es)
  Constrain (c ': es) = (c, Constrain es)

type ES m a = Eval m a

type Eval m a = ExceptT String (RWST Env Variables Steps m) a

data Error t

data Reader t

data State t

data Writer t

ask :: Monad m => Eval m Env
ask = lift RWS.ask

local :: Monad m => (Env -> Env) -> Eval m Value -> Eval m Value
local f = hoist (RWS.local f)

get :: Monad m => Eval m Steps
get = lift RWS.get

put :: Monad m => Steps -> Eval m ()
put = lift . RWS.put

tell :: Monad m => Variables -> Eval m ()
tell = lift . RWS.tell

throwError :: Monad m => String -> Eval m Value
throwError = throwE

normalize :: a -> a
normalize = id

-- effect

data Log

log :: MonadIO m => Text -> m ()
log = liftIO . putStrLn . unpack

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
