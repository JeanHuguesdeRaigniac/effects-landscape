module Internal.WithCleff
  ( module Cleff,
    module Cleff.Error,
    module Cleff.Reader,
    module Cleff.State,
    module Cleff.Writer,
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

import Cleff
import Cleff.Error hiding (throwError)
import qualified Cleff.Error as Error
import Cleff.Reader
import Cleff.State
import Cleff.Writer
import Commons (normalize3l, show)
import Data.Kind (Constraint)
import Data.Text (Text, unpack)
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
run = runIOE

runLog :: IOE :> es => Eff (Log : es) a -> Eff es a
runLog = interpret \case
  Log s -> liftIO $ putStrLn (unpack s)
