module Lib where

import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype M a = M a

unitM :: a -> M a
unitM = M

bindM :: M a -> (a -> M b) -> M b
bindM (M a) f = f a

showM :: M Value -> String
showM (M a) = showval a

type Name = String

data Term
  = Var Name
  | Con Int
  | Add Term Term
  | Lam Name Term
  | App Term Term

data Value
  = Wrong
  | Num Int
  | Fun (Value -> M Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = unitM (Num i)
interp (Add u v) e =
  interp u e
    `bindM` ( \a ->
                interp v e
                  `bindM` add a
            )
interp (Lam x v) e = unitM (Fun (\a -> interp v ((x, a) : e)))
interp (App t u) e =
  interp t e
    `bindM` ( \f ->
                interp u e
                  `bindM` apply f
            )

lookup :: Name -> Environment -> M Value
lookup x [] = unitM Wrong
lookup x ((y, b) : e) = if x == y then unitM b else lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM $ Num $ i + j
add _ _ = unitM Wrong

apply :: Value -> Value -> M Value
apply (Fun f) a = f a
apply _ _ = unitM Wrong

test :: Term -> String
test t = showM (interp t [])

term0 =
  App
    (Lam "x" (Add (Var "x") (Var "x")))
    (Add (Con 10) (Con 11))