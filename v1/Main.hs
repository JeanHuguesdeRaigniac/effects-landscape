module Main where

import App (runEval)
import Commons (getLibraryName, initialEnv, initialSteps, tests)

main :: IO ()
main = do
  putStrLn getLibraryName
  tests $ runEval initialEnv initialSteps
