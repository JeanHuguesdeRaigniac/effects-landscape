{-# LANGUAGE CPP #-}
#ifdef WITH_RWST_TRANSFORMERS
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module AppSpec where

import Commons (getLibraryName)
import qualified Data.Map as Map
import Test.Hspec
import Types
import Prelude hiding (log, show)

#ifdef WITH_CLEFF
import Internal.WithCleffSpec
#endif
#ifdef WITH_EFFECTFUL
import Internal.WithEffectfulSpec
#endif
#ifdef WITH_EXTENSIBLE_EFFECTS
import Internal.WithExtensibleEffectsSpec
#endif
#ifdef WITH_FREER_SIMPLE
import Internal.WithFreerSimpleSpec
#endif
#ifdef WITH_FUSED_EFFECTS
import Internal.WithFusedEffectsSpec
#endif
#ifdef WITH_HAS_TRANSFORMERS
import Internal.WithHasTransformersSpec
#endif
#ifdef WITH_MTL
import Internal.WithMtlSpec
#endif
#ifdef WITH_POLYSEMY
import Internal.WithPolysemySpec
#endif
#ifdef WITH_RIO
import Internal.WithRIOSpec
#endif
#ifdef WITH_RWST_MTL
import Internal.WithRWSTMtlSpec
#endif
#ifdef WITH_RWST_TRANSFORMERS
import Internal.WithRWSTTransformersSpec
#endif

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

runEval :: Env -> Steps -> Exp -> (Either String Value, Steps, Variables, Logs)
runEval env steps =
  normalize
    . run
    . runLogWriter
    . runVariableWriter
    . runState steps
    . runReader env
    . runError
    . eval

-- tests

spec :: Spec
spec =
  describe getLibraryName do
    describe "Wrong expression" AppSpec.wrongExpression
    describe "Right IntVal" AppSpec.rightIntVal
    describe "Right FunVal" AppSpec.rightFunVal

wrongExpression :: Spec
wrongExpression =
  it "returns a Left when given a wrong expression" do
    let (result, steps, variables, logs) =
          runEval Map.empty 0 $ Var "x"
    result `shouldBe` Left "unbound variable: x"
    steps `shouldBe` 1
    variables `shouldBe` ["x"]
    logs `shouldBe` []

rightIntVal :: Spec
rightIntVal =
  it "returns a Right when given a valid expression (IntVal)" do
    let (result, steps, variables, logs) =
          runEval Map.empty 0 $ Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)
    result `shouldBe` Right (IntVal 18)
    steps `shouldBe` 8
    variables `shouldBe` ["x"]
    logs
      `shouldBe` [ "Lit: 12",
                   "Lit: 4",
                   "Lit: 2",
                   "Step: 7",
                   "Current env: fromList []",
                   "Modified env: fromList [(\"x\",IntVal 6)]"
                 ]

rightFunVal :: Spec
rightFunVal =
  it "returns a Right when given a valid expression (FunVal)" do
    let (result, steps, variables, logs) =
          runEval Map.empty 0 $ Abs "x" (Var "x")
    result `shouldBe` Right (FunVal Map.empty "x" (Var "x"))
    steps `shouldBe` 1
    variables `shouldBe` []
    logs `shouldBe` []
