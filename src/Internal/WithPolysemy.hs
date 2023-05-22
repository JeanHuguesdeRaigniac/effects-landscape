{-# LANGUAGE PolyKinds #-}

module Internal.WithPolysemy
  ( module Polysemy,
    module Polysemy.Error,
    module Polysemy.Reader,
    module Polysemy.State,
    module Polysemy.Writer,
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

import Commons (Constrain, normalize3r, show)
import Data.Text (Text, unpack)
import Polysemy hiding (run)
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer
import Types (Value)
import Prelude hiding (log, show)

-- shoehorning

type C e sig es = Member e es

type CS es sig m = Constrain es

type ES m a = Sem m a

throwError :: Member (Error String) r => String -> Sem r Value
throwError = throw

normalize :: (c, (b, a)) -> (a, b, c)
normalize = normalize3r

-- effect

data Log m a where
  Log :: Text -> Log m ()

makeSem ''Log

-- interpreters

run :: Sem '[Embed IO] a -> IO a
run = runM

runLog :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
runLog = interpret \case
  Log s -> embed $ putStrLn (unpack s)
