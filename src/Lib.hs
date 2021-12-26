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

callccK :: ((a -> K b) -> K a) -> K a
callccK h c = let k a p = c a in h k c

type Name = String

data Term
  = Var Name
  | Con Int
  | Add Term Term
  | Lam Name Term
  | App Term Term
  | Callcc Name Term

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
interp (Callcc x v) e = callccK (\k -> interp v ((x, Fun k) : e))

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

term1 =
  Add
    (Con 1)
    ( Callcc
        "k"
        ( Add
            (Con 2)
            ( App
                (Var "k")
                (Add (Con 5) (Con 6))
            )
        )
    )