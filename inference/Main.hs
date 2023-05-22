module Main where

import Control.Monad.Freer (Eff, Member, reinterpret, runM)
import Control.Monad.Freer.State (State, execState, runState)
import qualified Control.Monad.Freer.State as State
import Control.Monad.Freer.TH (makeEffect)
import Debug.Trace (trace)

-- a warning in this case, a dreadful error message in others
tick :: Member (State Integer) effs => Eff effs ()
tick = do
  st <- State.get
  State.put (st + 1)

-- Solution 1
tickWithTypeAnnotation :: Member (State Integer) effs => Eff effs ()
tickWithTypeAnnotation = do
  st :: Integer <- State.get
  State.put (st + 1)

-- Solution 2
tickWithTypeApplication :: Member (State Integer) effs => Eff effs ()
tickWithTypeApplication = do
  st <- State.get @Integer
  State.put (st + 1)

-- Solution 3
-- get :: Member (State s) effs => Eff effs s
monomorphicGet :: Member (State Integer) effs => Eff effs Integer
monomorphicGet = State.get

tickWithMonomorphicGet :: Member (State Integer) effs => Eff effs ()
tickWithMonomorphicGet = do
  st <- monomorphicGet
  State.put (st + 1)

runCounter :: Integer -> Eff '[State Integer, IO] () -> IO Integer
runCounter steps =
  fmap (\i -> trace (show i) i) . runM . execState steps

-- Solution 4
data IntegerState r where
  Put :: Integer -> IntegerState ()
  Get :: IntegerState Integer

makeEffect ''IntegerState

runIntegerState ::
  forall effs w. Integer -> Eff (IntegerState ': effs) w -> Eff effs (w, Integer)
runIntegerState steps req = runState steps $ reinterpret go req
  where
    go :: IntegerState v -> Eff (State Integer ': effs) v
    go Get = State.get
    go (Put steps') = State.put steps'

tickWithIntegerState :: Member IntegerState effs => Eff effs ()
tickWithIntegerState = do
  st <- get
  put (st + 1)

runCounter' :: Integer -> Eff '[IntegerState, IO] a -> IO Integer
runCounter' steps = fmap snd . runM . runIntegerState steps

runInference :: IO ()
runInference = do
  one <- runCounter 0 tick
  two <- runCounter one tickWithTypeAnnotation
  three <- runCounter two tickWithTypeApplication
  four <- runCounter three tickWithMonomorphicGet
  five <- runCounter' four tickWithIntegerState
  print five

main :: IO ()
main = runInference
