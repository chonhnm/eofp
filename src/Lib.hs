module Lib where

import Data.Functor.Classes (eq1)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Foreign.C (e2BIG)

someFunc :: IO ()
someFunc = print "hello"

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

type Eval1 a = Identity a

instance MonadFail Identity where
  fail = error


runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Expr -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) =
  do
    IntVal i1 <- eval1 env e1
    IntVal i2 <- eval1 env e2
    return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) =
  do
    val1 <- eval1 env e1
    val2 <- eval1 env e2
    case val1 of
      FunVal env' n e -> eval1 (Map.insert n val2 env') e

b = runEval1 (eval1 Map.empty exampleExp)