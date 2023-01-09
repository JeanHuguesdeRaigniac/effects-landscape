module Main where

import qualified AppCleff
import qualified AppEffectful
import qualified AppExtensibleEffects
import qualified AppFreerSimple
import qualified AppFusedEffects
import qualified AppHasTransformers
import qualified AppMtl
import qualified AppPolysemy
import qualified AppRWSTMtl
import qualified AppRWSTTransformers
import qualified AppRio
import Commons (initialEnv, initialSteps, tests)
import Inference (runInference)

main :: IO ()
main = do
  putStrLn "0. original (mtl)"
  tests $ AppMtl.runEval initialEnv initialSteps . AppMtl.eval

  putStrLn "\n1. RWST mtl"
  tests $ AppRWSTMtl.runEval initialEnv initialSteps . AppRWSTMtl.eval

  putStrLn "\n2. RWST transformers"
  tests $ AppRWSTTransformers.runEval initialEnv initialSteps . AppRWSTTransformers.eval

  putStrLn "\n3. extensible-effects"
  tests $ AppExtensibleEffects.runEval initialEnv initialSteps . AppExtensibleEffects.eval

  putStrLn "\n4. freer-simple"
  tests $ AppFreerSimple.runEval initialEnv initialSteps . AppFreerSimple.eval

  putStrLn "\n5. rio"
  tests $ AppRio.runEval initialEnv initialSteps . AppRio.eval

  putStrLn "\n6. fused-effects"
  tests $ AppFusedEffects.runEval initialEnv initialSteps . AppFusedEffects.eval

  putStrLn "\n7. polysemy"
  tests $ AppPolysemy.runEval initialEnv initialSteps . AppPolysemy.eval

  putStrLn "\n8. effectful"
  tests $ AppEffectful.runEval initialEnv initialSteps . AppEffectful.eval

  putStrLn "\n9. has-transformers"
  tests $ AppHasTransformers.runEval initialEnv initialSteps . AppHasTransformers.eval

  putStrLn "\n10. cleff"
  tests $ AppCleff.runEval initialEnv initialSteps . AppCleff.eval

  putStrLn "\nInference"
  runInference
