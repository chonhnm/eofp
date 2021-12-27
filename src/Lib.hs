module Lib where

import qualified Data.Map as Map 

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
   
