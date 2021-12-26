module Lib where

import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Answer = Value

type K a = (a -> Answer) -> Answer

unitK :: a -> K a
unitK a c = c a

bindK :: K a -> (a -> K b) -> K b
bindK m f c = m (`f` c)


showK :: K Value -> String
showK m = showval (m id)

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
  | Fun (Value -> K Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> K Value
interp (Var x) e = lookup x e
interp (Con i) e = unitK (Num i)
interp (Add u v) e =
  interp u e
    `bindK` ( \a ->
                interp v e
                  `bindK` add a
            )
interp (Lam x v) e = unitK (Fun (\a -> interp v ((x, a) : e)))
interp (App t u) e =
  interp t e
    `bindK` ( \f ->
                interp u e
                  `bindK` apply f
            )

lookup :: Name -> Environment -> K Value
lookup x [] = unitK Wrong
lookup x ((y, b) : e) = if x == y then unitK b else lookup x e

add :: Value -> Value -> K Value
add (Num i) (Num j) = unitK $ Num $ i + j
add _ _ = unitK Wrong

apply :: Value -> Value -> K Value
apply (Fun f) a = f a
apply _ _ = unitK Wrong

test :: Term -> String
test t = showK (interp t [])

term0 =
  App
    (Lam "x" (Add (Var "x") (Var "x")))
    (Add (Con 10) (Con 11))