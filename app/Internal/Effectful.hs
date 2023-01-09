module Internal.Effectful
  ( module Effectful,
    module Effectful.Error.Static,
    module Effectful.Reader.Static,
    module Effectful.State.Static.Local,
    module Effectful.Writer.Static.Local,
    -- State
    get,
  )
where

import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local hiding (get)
import qualified Effectful.State.Static.Local as State
import Effectful.Writer.Static.Local
import Types (Steps)

get :: State Steps :> es => Eff es Steps
get = State.get
