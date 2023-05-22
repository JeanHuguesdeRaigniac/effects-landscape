module Internal.WithFreerSimple
  ( module Control.Monad.Freer,
    module Control.Monad.Freer.Error,
    module Control.Monad.Freer.Reader,
    module Control.Monad.Freer.State,
    module Control.Monad.Freer.Writer,
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

import Commons (normalize3l, show)
import Control.Monad.Freer hiding (run)
import qualified Control.Monad.Freer as Freer
import Control.Monad.Freer.Error hiding (throwError)
import qualified Control.Monad.Freer.Error as Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.TH
import Control.Monad.Freer.Writer
import Data.Kind (Constraint)
import Data.Text (Text, unpack)
import Prelude hiding (log, show)

-- shoehorning

type C e sig es = Member e es

type CS es sig m = Constrain es m

type family Constrain es m :: Constraint where
  Constrain '[] _ = ()
  Constrain (C e _ _ ': es) m = (Member e m, Constrain es m)

type ES m a = Eff m a

throwError :: Member (Error String) effs => String -> Eff effs a
throwError = Error.throwError

normalize :: ((a, b), c) -> (a, b, c)
normalize = normalize3l

-- effect

data Log r where
  Log :: Text -> Log ()

makeEffect ''Log

-- interpreters

run :: Eff '[IO] a -> IO a
run = Freer.runM

runLog ::
  forall effs a.
  LastMember IO effs =>
  Eff (Log ': effs) a ->
  Eff effs a
runLog = interpretM \case
  Log s -> putStrLn (unpack s)
