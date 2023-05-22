module Internal.WithEffectful
  ( module Effectful,
    module Effectful.Error.Static,
    module Effectful.Reader.Static,
    module Effectful.State.Static.Local,
    module Effectful.Writer.Static.Local,
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
    runError,
  )
where

import Commons (normalize3l, show)
import Data.Kind (Constraint)
import Data.Text (Text, unpack)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static hiding (runError, throwError)
import qualified Effectful.Error.Static as Error
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.TH
import Effectful.Writer.Static.Local
import Prelude hiding (log, show)

-- shoehorning

type C e sig es = e :> es

type CS es sig m = Constrain es m

type family Constrain es m :: Constraint where
  Constrain '[] _ = ()
  Constrain (C e _ _ ': es) m = (e :> m, Constrain es m)

type ES m a = Eff m a

throwError :: Error String :> es => String -> Eff es a
throwError = Error.throwError

normalize :: ((a, b), c) -> (a, b, c)
normalize = normalize3l

-- effect

data Log :: Effect where
  Log :: Text -> Log m ()

makeEffect ''Log

-- interpreters

run :: Eff '[IOE] a -> IO a
run = runEff

runLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runLog = interpret $ \_ -> \case
  Log s -> liftIO $ putStrLn (unpack s)

runError :: Eff (Error e : es) a -> Eff es (Either e a)
runError = runErrorNoCallStack
