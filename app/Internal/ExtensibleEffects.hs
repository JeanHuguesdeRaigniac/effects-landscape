module Internal.ExtensibleEffects
  ( module Control.Eff,
    module Control.Eff.Exception,
    module Control.Eff.Reader.Lazy,
    module Control.Eff.State.Lazy,
    module Control.Eff.Trace,
    module Control.Eff.Writer.Lazy,
    -- Trace
    embeddedLog,
    -- State
    get,
  )
where

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy hiding (get)
import qualified Control.Eff.State.Lazy as State
import Control.Eff.Trace
import Control.Eff.Writer.Lazy
import Types (Steps)

embeddedLog :: Member Trace r => String -> Eff r ()
embeddedLog = trace

get :: Member (State Steps) r => Eff r Steps
get = State.get
