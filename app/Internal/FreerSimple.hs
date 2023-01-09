module Internal.FreerSimple
  ( module Control.Monad.Freer,
    module Control.Monad.Freer.Error,
    module Control.Monad.Freer.Reader,
    module Control.Monad.Freer.State,
    module Control.Monad.Freer.Trace,
    module Control.Monad.Freer.Writer,
    -- Trace
    embeddedLog,
    -- State
    get,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State hiding (get)
import qualified Control.Monad.Freer.State as State
import Control.Monad.Freer.Trace
import Control.Monad.Freer.Writer
import Types (Steps)

embeddedLog :: Member Trace effs => String -> Eff effs ()
embeddedLog = trace

get :: Member (State Steps) effs => Eff effs Steps
get = State.get
