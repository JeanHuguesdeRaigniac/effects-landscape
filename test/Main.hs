module Main where

import qualified AppSpec
import Test.Hspec

main :: IO ()
main = hspec AppSpec.spec
