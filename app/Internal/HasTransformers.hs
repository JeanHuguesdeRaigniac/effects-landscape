module Internal.HasTransformers
  ( module Control.Monad.Trans.Has,
    module Control.Monad.Trans.Has.Except,
    module Control.Monad.Trans.Has.Reader,
    module Control.Monad.Trans.Has.State,
    module Control.Monad.Trans.Has.Writer,
    Control.Monad.IO.Class.MonadIO,
    Control.Monad.Trans.Except.runExceptT,
    -- Error
    throwError,
    -- Reader
    local,
    -- State
    get,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Has
import Control.Monad.Trans.Has.Except
import Control.Monad.Trans.Has.Reader
import Control.Monad.Trans.Has.State hiding (get)
import qualified Control.Monad.Trans.Has.State as State
import Control.Monad.Trans.Has.Writer
import qualified Control.Monad.Trans.Reader as Reader
import Types (Env, Steps, Value)

throwError :: HasExcept String m => String -> m Value
throwError e = liftH $ ExceptT $ return $ Left e

local ::
  (MFunctor t, Monad m) =>
  (Env -> Env) ->
  t (ReaderT Env m) Value ->
  t (ReaderT Env m) Value
local f = hoist (Reader.local f)

get :: HasState Steps m => m Steps
get = State.get
