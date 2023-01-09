module Internal.Cleff
  ( module Cleff,
    module Cleff.Error,
    module Cleff.Reader,
    module Cleff.State,
    module Cleff.Writer,
    -- State
    get,
  )
where

import Cleff
import Cleff.Error
import Cleff.Reader
import Cleff.State hiding (get)
import qualified Cleff.State as State
import Cleff.Writer
import Types (Steps)

get :: State Steps :> es => Eff es Steps
get = State.get
