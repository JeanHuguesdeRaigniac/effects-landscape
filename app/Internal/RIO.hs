module Internal.RIO
  ( RIO.RIO,
    (RIO.^.),
    RIO.first,
    RIO.liftA3,
    RIO.newIORef,
    RIO.readIORef,
    RIO.runRIO,
    RIO.try,
    RIOApp (..),
    HasEnv,
    HasSteps,
    HasVariables,
    EvalException (..),
    -- lenses
    stepsL,
    varsL,
    -- Error
    throwError,
    -- Reader
    ask,
    local,
    -- State
    get,
    put,
    -- Writer
    tell,
  )
where

import qualified RIO
import Types

data RIOApp = RIOApp
  { appEnv :: Env,
    appSteps :: !(RIO.IORef Steps),
    appVariables :: !(RIO.IORef Variables)
  }

class HasEnv env where
  envL :: RIO.Lens' env Env

instance HasEnv RIOApp where
  envL = RIO.lens appEnv (\x y -> x {appEnv = y})

class HasSteps env where
  stepsL :: RIO.Lens' env (RIO.IORef Steps)

instance HasSteps RIOApp where
  stepsL = RIO.lens appSteps (\x y -> x {appSteps = y})

class HasVariables env where
  varsL :: RIO.Lens' env (RIO.IORef Variables)

instance HasVariables RIOApp where
  varsL = RIO.lens appVariables (\x y -> x {appVariables = y})

newtype EvalException = EvalException String
  deriving (RIO.Typeable)

instance Show EvalException where
  show (EvalException msg) = msg

instance RIO.Exception EvalException

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
