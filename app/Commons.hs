module Commons where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Types

initialSteps :: Steps
initialSteps = 0

initialEnv :: Env
initialEnv = Map.empty

-- reaching buried IO by lifting it to the top of our Transformers' stack
-- liftIO spares us having to go manually through each stack level with lift
embeddedLog :: MonadIO m => String -> m ()
embeddedLog = liftIO . putStrLn

test :: (Exp -> IO (Either String Value, Steps, Variables)) -> Exp -> IO ()
test runTrans exp' = do
  putStrLn $ "input:" <> show exp'

  putStrLn "\nEvaluation:"
  raw@(result, steps, variables) <- runTrans exp'

  putStrLn $ "\noutput:" <> show raw
  putStrLn $ "\nSteps: " <> show steps
  putStrLn $ "Variables: " <> show variables
  putStrLn $ "Result: " <> show result

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
