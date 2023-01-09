module Internal.RWSTTransformers
  ( module Control.Monad.Morph,
    module Control.Monad.Trans.Except,
    module Control.Monad.Trans.RWS.Lazy,
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

import Control.Monad.Morph
import Control.Monad.Trans.Except hiding (liftCallCC)
import Control.Monad.Trans.RWS.Lazy hiding (ask, get, local, put, tell)
import qualified Control.Monad.Trans.RWS.Lazy as RWS
import Types (Env, Steps, Value, Variables)

type Eval a = ExceptT String (RWST Env Variables Steps IO) a

throwError :: String -> Eval Value
throwError = throwE

ask :: Eval Env
ask = lift RWS.ask

local :: (Env -> Env) -> Eval Value -> Eval Value
local f = hoist (RWS.local f)

get :: Eval Steps
get = lift RWS.get

put :: Steps -> Eval ()
put = lift . RWS.put

tell :: Variables -> Eval ()
tell = lift . RWS.tell
