{-# LANGUAGE CPP #-}
#ifdef WITH_RWST_TRANSFORMERS
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module App where

import qualified Data.Map as Map
#ifdef WITH_CLEFF
import Internal.WithCleff
#endif
#ifdef WITH_EFFECTFUL
import Internal.WithEffectful
#endif
#ifdef WITH_EFFET
import Internal.WithEffet
#endif
#ifdef WITH_EXTENSIBLE_EFFECTS
import Internal.WithExtensibleEffects
#endif
#ifdef WITH_FREER_SIMPLE
import Internal.WithFreerSimple
#endif
#ifdef WITH_FUSED_EFFECTS
import Internal.WithFusedEffects
#endif
#ifdef WITH_HAS_TRANSFORMERS
import Internal.WithHasTransformers
#endif
#ifdef WITH_MTL
import Internal.WithMtl
#endif
#ifdef WITH_POLYSEMY
import Internal.WithPolysemy
#endif
#ifdef WITH_RIO
import Internal.WithRIO
#endif
#ifdef WITH_RWST_MTL
import Internal.WithRWSTMtl
#endif
#ifdef WITH_RWST_TRANSFORMERS
import Internal.WithRWSTTransformers
#endif
import Types
import Prelude hiding (log, show)

-- application functions

tick :: C (State Steps) sig m => ES m ()
tick = do
  st :: Integer <- get
  put (st + 1)

eval ::
  CS
    '[ C Log sig m,
       C (Error String) sig m,
       C (Reader Env) sig m,
       C (State Steps) sig m,
       C (Writer Variables) sig m
     ]
    sig
    m =>
  Exp ->
  ES m Value
eval (Lit i) = do
  tick
  log ("Lit: " <> show i)
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
      step :: Integer <- get
      log ("Step: " <> show step)
      log ("Current env: " <> show env')
      log ("Modified env: " <> show (Map.insert n val2 env'))
      local (const (Map.insert n val2 env')) (eval body)
    IntVal _ -> throwError "type error in application"

-- interpreters

runEval :: Env -> Steps -> Exp -> IO (Either String Value, Steps, Variables)
runEval env steps =
  fmap normalize
    <$> run
      . runLog
      . runWriter
      . runState steps
      . runReader env
      . runError
      . eval
