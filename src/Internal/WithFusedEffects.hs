{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.WithFusedEffects
  ( module Control.Carrier.Error.Either,
    module Control.Carrier.Lift,
    module Control.Carrier.Reader,
    module Control.Carrier.State.Strict,
    module Control.Carrier.Writer.Strict,
    Control.Monad.IO.Class.MonadIO,
    -- shoehorning
    C,
    CS,
    ES,
    throwError,
    normalize,
    show,
    -- effect
    Log (..),
    log,
    -- interpreters
    run,
    runLog,
  )
where

import Commons (Constrain, normalize3r, show)
import Control.Algebra hiding (run)
import Control.Carrier.Error.Either hiding (run, throwError)
import qualified Control.Carrier.Error.Either as Either
import Control.Carrier.Lift hiding (run)
import Control.Carrier.Reader hiding (run)
import Control.Carrier.State.Strict hiding (run)
import Control.Carrier.Writer.Strict hiding (run)
import Control.Effect.Sum
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Kind (Type)
import Data.Text (Text, unpack)
import Prelude hiding (log, show)

-- shoehorning

type C e sig m = Has e sig m

type CS es sig m = (Algebra sig m, Monad m, Constrain es)

type ES m a = m a

throwError :: (Member (Throw String) sig, Algebra sig m) => String -> m a
throwError = Either.throwError

normalize :: (c, (b, a)) -> (a, b, c)
normalize = normalize3r

-- effect

data Log (m :: Type -> Type) k where
  Log :: Text -> Log m ()

log :: Has Log sig m => Text -> m ()
log = send . Log

newtype LogIOC m a = LogIOC {runLogIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Log :+: sig) (LogIOC m) where
  alg hdl sig ctx = case sig of
    L (Log s) -> ctx <$ liftIO (putStrLn (unpack s))
    R other -> LogIOC (alg (runLogIO . hdl) other ctx)

-- interpreters

run :: LiftC m a -> m a
run = runM

runLog :: LogIOC m a -> m a
runLog = runLogIO
