{-# LANGUAGE CPP #-}

module Commons where

import Data.Kind (Constraint)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Types
import Prelude hiding (exp)

type family Constrain es :: Constraint where
  Constrain '[] = ()
  Constrain (c ': es) = (c, Constrain es)

show :: Show a => a -> Text
show = pack . Prelude.show

normalize3l :: ((a, b), c) -> (a, b, c)
normalize3l ((result, steps), variables) = (result, steps, variables)

normalize3r :: (c, (b, a)) -> (a, b, c)
normalize3r (variables, (steps, result)) = (result, steps, variables)

normalize4l :: (((a, b), c), d) -> (a, b, c, d)
normalize4l (((result, steps), variables), logs) = (result, steps, variables, logs)

normalize4r :: (d, (c, (b, a))) -> (a, b, c, d)
normalize4r (logs, (variables, (steps, result))) = (result, steps, variables, logs)

initialSteps :: Steps
initialSteps = 0

initialEnv :: Env
initialEnv = Map.empty

getLibraryName :: String
getLibraryName = {- ORMOLU_DISABLE -}
#ifdef WITH_CLEFF
  "cleff"
#endif
#ifdef WITH_EFFECTFUL
  "effectful"
#endif
#ifdef WITH_EXTENSIBLE_EFFECTS
  "extensible-effects"
#endif
#ifdef WITH_FREER_SIMPLE
  "freer-simple"
#endif
#ifdef  WITH_FUSED_EFFECTS
   "fused-effects"
#endif
#ifdef  WITH_HAS_TRANSFORMERS
  "has-transformers"
#endif
#ifdef  WITH_MTL
   "mtl"
#endif
#ifdef  WITH_POLYSEMY
   "polysemy"
#endif
#ifdef  WITH_RIO
   "rio"
#endif
#ifdef  WITH_RWST_MTL
   "rwst-mtl"
#endif
#ifdef  WITH_RWST_TRANSFORMERS
   "rwst-transformers"
#endif
{- ORMOLU_ENABLE -}

test :: (Exp -> IO (Either String Value, Steps, Variables)) -> Exp -> IO ()
test runTrans exp = do
  putStrLn $ "input:" <> Prelude.show exp

  putStrLn "\nEvaluation:"
  raw@(result, steps, variables) <- runTrans exp

  putStrLn $ "\noutput:" <> Prelude.show raw
  putStrLn $ "\nSteps: " <> Prelude.show steps
  putStrLn $ "Variables: " <> Prelude.show variables
  putStrLn $ "Result: " <> Prelude.show result

tests :: (Exp -> IO (Either String Value, Steps, Variables)) -> IO ()
tests runTransformers = do
  let test' = test runTransformers
  putStrLn "Wrong expression"
  putStrLn "----------------"
  test' $ Var "x"
  -- output: ((Left "unbound variable: x",["x"]),1)

  putStr "\n\n"
  putStrLn "Valid expression (IntVal)"
  putStrLn "-------------------------"
  test' $ Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)
  -- output: ((Right (IntVal 18),["x"]),8)

  putStr "\n\n"
  putStrLn "Valid expression (FunVal)"
  putStrLn "-------------------------"
  test' $ Abs "x" (Var "x")

-- output: ((Right (FunVal (fromList []) "x" (Var "x")),[]),1)

-- other examples
-- test' $ App (Abs "x" (Lit 42)) (Abs "y" (Var "y"))
-- test' $ App (Abs "y" (Var "y")) (Abs "x" (Lit 42))
