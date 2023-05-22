{-# LANGUAGE PolyKinds #-}

module Internal.WithRIO
  ( -- shoehorning
    C,
    CS,
    ES,
    Error,
    EvalException (..),
    HasEnv (..),
    HasSteps (..),
    HasVariables (..),
    Id,
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

import Commons (Constrain)
import RIO (HasLogFunc, first, liftA2, liftA3, newIORef, readIORef, runRIO, try, (^.))
import qualified RIO
import Types
import Prelude hiding (log, show)
import qualified Prelude

-- shoehorning

type C e p m = (Reify e) m

type family Reify e where
  Reify Log = HasLogFunc
  Reify (Error t) = Id
  Reify (Reader t) = HasEnv
  Reify (State t) = HasSteps
  Reify (Writer t) = HasVariables

type CS es sig m = Constrain es

type ES m a = RIO.RIO m a

data RIOApp = RIOApp
  { appLogFunc :: !RIO.LogFunc,
    appEnv :: Env,
    appSteps :: !(RIO.IORef Steps),
    appVariables :: !(RIO.IORef Variables)
  }

class Id a

instance Id RIOApp

data Error t

data Reader t

data State t

data Writer t

throwError :: String -> RIO.RIO env Value
throwError msg = RIO.throwM (EvalException msg)

normalize :: a -> a
normalize = id

class HasEnv env where
  envL :: RIO.Lens' env Env

instance HasEnv RIOApp where
  envL = RIO.lens appEnv (\x y -> x {appEnv = y})

ask :: HasEnv env => RIO.RIO env Env
ask = RIO.view envL

local :: HasEnv env => (Env -> Env) -> RIO.RIO env Value -> RIO.RIO env Value
local f ev = do
  app <- RIO.ask
  RIO.runRIO app $ RIO.local (RIO.over envL f) ev

class HasSteps env where
  stepsL :: RIO.Lens' env (RIO.IORef Steps)

instance HasSteps RIOApp where
  stepsL = RIO.lens appSteps (\x y -> x {appSteps = y})

get :: HasSteps env => RIO.RIO env Steps
get = do
  stepsRef <- RIO.view stepsL
  RIO.liftIO $ RIO.readIORef stepsRef

put :: HasSteps env => Steps -> RIO.RIO env ()
put newSteps = do
  stepsRef <- RIO.view stepsL
  RIO.liftIO $ RIO.writeIORef stepsRef newSteps

class HasVariables env where
  varsL :: RIO.Lens' env (RIO.IORef Variables)

instance HasVariables RIOApp where
  varsL = RIO.lens appVariables (\x y -> x {appVariables = y})

tell :: HasVariables env => Variables -> RIO.RIO env ()
tell variables = do
  varsRef <- RIO.view varsL
  RIO.liftIO $ RIO.modifyIORef' varsRef (variables ++)

newtype EvalException = EvalException String
  deriving (RIO.Typeable)

instance Show EvalException where
  show (EvalException msg) = msg

instance RIO.Exception EvalException

-- effect

data Log

instance HasLogFunc RIOApp where
  logFuncL = RIO.lens appLogFunc (\x y -> x {appLogFunc = y})

log :: RIO.HasLogFunc env => RIO.Utf8Builder -> RIO.RIO env ()
log = RIO.logInfo

show :: Show a => a -> RIO.Utf8Builder
show = RIO.displayShow

-- interpreters

run :: a -> a
run = id

runLog :: a -> a
runLog = id

runWriter :: a -> a
runWriter = id

runState :: s -> (s -> a) -> a
runState s f = f s

runReader ::
  RIO.MonadUnliftIO m =>
  Env ->
  RIO.RIO RIOApp Value ->
  Steps ->
  m (Either String Value, Steps, Variables)
runReader env ev steps = do
  logOptions' <- RIO.logOptionsHandle RIO.stdout False
  let logOptions = RIO.setLogUseTime False $ RIO.setLogUseLoc False logOptions'
  RIO.withLogFunc logOptions $ \logFunc -> do
    app <- liftA2 (RIOApp logFunc env) (newIORef steps) (newIORef [])
    liftA3
      (,,)
      (first (Prelude.show @EvalException) <$> try (runRIO app ev))
      (readIORef (app ^. stepsL)) -- app ^. stepsL <=> view stepsL app
      (readIORef (app ^. varsL))

runError :: a -> a
runError = id
