module AppCleff where

import Commons (embeddedLog)
import qualified Data.Map as Map
import Internal.Cleff
import Types

type Eval a = Eff '[Error String, Reader Env, State Steps, Writer Variables, IOE] a

runEval :: Env -> Steps -> Eval Value -> IO (Either String Value, Steps, Variables)
runEval env steps ev =
  (\((result, steps'), variables) -> (result, steps', variables))
    <$> runIOE (runWriter (runState steps (runReader env (runError ev))))

tick :: State Steps :> es => Eff es ()
tick = do
  st <- get
  put (st + 1)

eval ::
  ( State Steps :> es,
    Writer Variables :> es,
    Reader Env :> es,
    Error String :> es,
    IOE :> es
  ) =>
  Exp ->
  Eff es Value
eval (Lit i) = do
  tick
  embeddedLog ("Lit: " <> show i)
  return $ IntVal i
eval (Var n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval (Plus e1 e2) = do
  tick
  e1' <- eval e1
  e2' <- eval e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) ->
      return $ IntVal (i1 + i2)
    _anyOtherCombination -> throwError "type error in addition"
eval (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval (App e1 e2) = do
  tick
  val1 <- eval e1
  val2 <- eval e2
  case val1 of
    FunVal env' n body -> do
      step <- get
      embeddedLog ("Step: " <> show step)
      embeddedLog ("Current env: " <> show env')
      embeddedLog ("Modified env: " <> show (Map.insert n val2 env'))
      local (const (Map.insert n val2 env')) (eval body)
    IntVal _ -> throwError "type error in application"
