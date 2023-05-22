module Types where

import qualified Data.Map as Map

type Name = String -- variable names

data Exp
  = Lit Integer -- expressions
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show)

data Value
  = IntVal Integer -- values
  | FunVal Env Name Exp
  deriving (Show)

type Env = Map.Map Name Value -- mapping from names to values

-- aliases for readability
type Steps = Integer

type Variables = [Name]
