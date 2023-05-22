{-# LANGUAGE PolyKinds #-}

module Internal.WithRIOSpec
  ( -- shoehorning
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
    runLogWriter,
    runVariableWriter,
    runState,
    runReader,
    runError,
  )
where

import Commons (show)
import Data.Text (Text)
import Internal.WithRIO (CS, ES, Error, EvalException (..), HasEnv (..), HasSteps (..), HasVariables (..), Id, Log, Reader, State, Writer, normalize, runError, runState)
import RIO (first, liftA3, newIORef, readIORef, runRIO, try, (^.))
import qualified RIO
import System.IO.Unsafe (unsafePerformIO)
import Types
import Prelude hiding (log, show)
import qualified Prelude (show)

-- shoehorning

type C e p m = (Reify e) m

type family Reify e where
  Reify Log = HasLogs
  Reify (Error t) = Id
  Reify (Reader t) = HasEnv
  Reify (State t) = HasSteps
  Reify (Writer t) = HasVariables

data RIOApp = RIOApp
  { appEnv :: Env,
    appSteps :: !(RIO.IORef Steps),
    appVariables :: !(RIO.IORef Variables),
    appLogs :: !(RIO.IORef Logs)
  }

-- identical boilerplate needed because RIOApp has changed

instance Id RIOApp

instance HasEnv RIOApp where
  envL = RIO.lens appEnv (\x y -> x {appEnv = y})

instance HasSteps RIOApp where
  stepsL = RIO.lens appSteps (\x y -> x {appSteps = y})

instance HasVariables RIOApp where
  varsL = RIO.lens appVariables (\x y -> x {appVariables = y})

throwError :: String -> RIO.RIO env Value
throwError msg = RIO.throwM (EvalException msg)

ask :: HasEnv env => RIO.RIO env Env
ask = RIO.view envL

local :: HasEnv env => (Env -> Env) -> RIO.RIO env Value -> RIO.RIO env Value
local f ev = do
  app <- RIO.ask
  RIO.runRIO app $ RIO.local (RIO.over envL f) ev

get :: HasSteps env => RIO.RIO env Steps
get = do
  stepsRef <- RIO.view stepsL
  RIO.liftIO $ RIO.readIORef stepsRef

put :: HasSteps env => Steps -> RIO.RIO env ()
put newSteps = do
  stepsRef <- RIO.view stepsL
  RIO.liftIO $ RIO.writeIORef stepsRef newSteps

tell :: HasVariables env => Variables -> RIO.RIO env ()
tell variables = do
  varsRef <- RIO.view varsL
  RIO.liftIO $ RIO.modifyIORef' varsRef (variables ++)

-- effect

class HasLogs env where
  logsL :: RIO.Lens' env (RIO.IORef Logs)

instance HasLogs RIOApp where
  logsL = RIO.lens appLogs (\x y -> x {appLogs = y})

log :: HasLogs env => Text -> RIO.RIO env ()
log s = do
  logsRef <- RIO.view logsL
  RIO.liftIO $ RIO.modifyIORef' logsRef (++ [s])

-- interpreters

run :: IO a -> a
run = unsafePerformIO

runLogWriter :: a -> a
runLogWriter = id

runVariableWriter :: a -> a
runVariableWriter = id

runReader ::
  RIO.MonadUnliftIO m =>
  Env ->
  RIO.RIO RIOApp Value ->
  Steps ->
  m (Either String Value, Steps, Variables, Logs)
runReader env ev steps = do
  app <- liftA3 (RIOApp env) (newIORef steps) (newIORef []) (newIORef [])
  liftA4
    (,,,)
    (first (Prelude.show @EvalException) <$> try (runRIO app ev))
    (readIORef (app ^. stepsL)) -- app ^. stepsL <=> view stepsL app
    (readIORef (app ^. varsL))
    (readIORef (app ^. logsL))
  where
    liftA4 f a b c d = liftA3 f a b c <*> d
