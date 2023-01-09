module Internal.FusedEffects
  ( module Control.Carrier.Error.Either,
    module Control.Carrier.Lift,
    module Control.Carrier.Reader,
    module Control.Carrier.State.Strict,
    module Control.Carrier.Writer.Strict,
    Control.Monad.IO.Class.MonadIO,
    -- State
    get,
  )
where

import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict hiding (get)
import qualified Control.Carrier.State.Strict as State
import Control.Carrier.Writer.Strict
import Control.Monad.IO.Class (MonadIO)
import Types (Steps)

get :: Has (State Steps) sig m => m Steps
get = State.get
