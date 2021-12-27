module Lib where

import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Name = String

data Expr
  = Lit Integer
  | Var Name
  | Plus Expr Expr
  | Abs Name Expr
  | App Expr Expr
  deriving (Show)

data Value
  = IntVal Integer
  | FunVal Env Name Expr
  deriving (Show)

type Env = Map.Map Name Value

eval0 :: Env -> Expr -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) =
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
   in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
  let val1 = eval0 env e1
      val2 = eval0 env e2
   in case val1 of
        FunVal env' n e -> eval0 (Map.insert n val2 env') e
        _ -> error "not function"

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)    

