{-# LANGUAGE UndecidableInstances #-}

module Internal.WithExtensibleEffects
  ( module Control.Eff,
    module Control.Eff.Exception,
    module Control.Eff.Reader.Lazy,
    module Control.Eff.State.Lazy,
    module Control.Eff.Writer.Lazy,
    -- shoehorning
    C,
    CS,
    ES,
    Error,
    throwError,
    normalize,
    show,
    -- effect
    Log,
    Log' (..),
    log,
    -- interpreters
    run,
    runLog,
    runWriter,
  )
where

import Commons (normalize3l, show)
import Control.Eff hiding (run)
import Control.Eff.Exception hiding (throwError)
import qualified Control.Eff.Exception as Exception
import Control.Eff.Operational
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Writer.Lazy hiding (runWriter)
import Data.Kind (Constraint, Type)
import Data.Text (Text, unpack)
import Types (Variables)
import Prelude hiding (log, show)

-- shoehorning

type C e sig es = Member e es

type CS es sig m = Constrain es m

type family Constrain es m :: Constraint where
  Constrain '[] _ = ()
  Constrain (C e _ _ ': es) m = (Member e m, Constrain es m)

type ES m a = Eff m a

type Error = Exc

throwError :: forall (r :: [Type -> Type]) a. Member (Exc String) r => String -> Eff r a
throwError = Exception.throwError

normalize :: ((a, b), c) -> (a, b, c)
normalize = normalize3l

-- effect

type Log = Program Log'

data Log' v where
  Log :: Text -> Log' ()

log :: Member (Program Log') r => Text -> Eff r ()
log = singleton . Log

-- interpreters

run :: Eff '[Lift IO] w -> IO w
run = runLift

runLog :: SetMember Lift (Lift IO) r => Eff (Program Log' : r) a -> Eff r a
runLog = runProgram \case
  Log s -> lift $ putStrLn (unpack s)

runWriter :: Eff (Writer Variables : r) a -> Eff r (a, Variables)
runWriter = runMonoidWriter @Variables
