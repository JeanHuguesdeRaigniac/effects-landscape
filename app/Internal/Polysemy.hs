module Internal.Polysemy
  ( module Polysemy,
    module Polysemy.Error,
    module Polysemy.Reader,
    module Polysemy.State,
    module Polysemy.Trace,
    module Polysemy.Writer,
    -- Trace
    embeddedLog,
    -- Error
    throwError,
    -- State
    get,
  )
where

import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State hiding (get)
import qualified Polysemy.State as State
import Polysemy.Trace
import Polysemy.Writer
import Types (Steps, Value)

embeddedLog :: Member Trace r => String -> Sem r ()
embeddedLog = trace

throwError :: Member (Error String) r => String -> Sem r Value
throwError = throw

get :: Member (State Steps) r => Sem r Steps
get = State.get
