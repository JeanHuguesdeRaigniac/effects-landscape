module Types where

import qualified Data.Map as Map
import Data.Text (Text)

type Name = String -- variable names

data Exp
  = Lit Integer -- expressions
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Eq, Show)

data Value
  = IntVal Integer -- values
  | FunVal Env Name Exp
  deriving (Eq, Show)

type Env = Map.Map Name Value -- mapping from names to values

-- aliases for readability
type Steps = Integer

type Variables = [Name]

type Logs = [Text]
