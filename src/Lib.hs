module Lib where

import Control.Monad.Except (Except, ExceptT, MonadError (throwError), runExceptT, throwError)
import Control.Monad.Reader
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Data.Functor.Classes (eq1)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

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

exampleExpErr = Lit 12 `Plus` App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2)

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

t1 = runEval1 (eval1 Map.empty exampleExp)

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

eval2 :: Env -> Expr -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound variable: " ++ n
    Just a -> return a
eval2 env (Plus e1 e2) =
  do
    val1 <- eval2 env e1
    val2 <- eval2 env e2
    case (val1, val2) of
      (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
      _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) =
  do
    val1 <- eval2 env e1
    val2 <- eval2 env e2
    case val1 of
      FunVal env' n e -> eval2 (Map.insert n val2 env') e
      _ -> throwError "type error in application"

t2 = runEval2 (eval2 Map.empty exampleExp)

t2b = runEval2 (eval2 Map.empty exampleExpErr)

type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity $ runExceptT $ runReaderT ev env

eval3 :: Expr -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) =
  do
    env <- ask
    case Map.lookup n env of
      Nothing -> throwError $ "unbound variable: " ++ n
      Just a -> return a
eval3 (Plus e1 e2) =
  do
    val1 <- eval3 e1
    val2 <- eval3 e2
    case (val1, val2) of
      (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
      _ -> throwError "type error in addition"
eval3 (Abs n e) =
  do
    env <- ask
    return $ FunVal env n e
eval3 (App e1 e2) =
  do
    val1 <- eval3 e1
    val2 <- eval3 e2
    case val1 of
      FunVal env' n e -> local (const (Map.insert n val2 env')) (eval3 e)
      _ -> throwError "type error in application"

t3 :: Either String Value
t3 = runEval3 Map.empty (eval3 exampleExp)

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st m = runIdentity (runStateT (runExceptT (runReaderT m env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval4 :: Expr -> Eval4 Value
eval4 (Lit n) =
  do
    tick
    return $ IntVal n
eval4 (Var n) =
  do
    tick
    env <- ask
    case Map.lookup n env of
      Nothing -> throwError $ "unbound variable: " ++ n
      Just a -> return a
eval4 (Plus e1 e2) =
  do
    tick
    val1 <- eval4 e1
    val2 <- eval4 e2
    case (val1, val2) of
      (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
      _ -> throwError "type error in addition"
eval4 (Abs n e) =
  do
    tick
    env <- ask
    return $ FunVal env n e
eval4 (App e1 e2) =
  do
    tick
    val1 <- eval4 e1
    val2 <- eval4 e2
    case val1 of
      FunVal env' n e -> local (const (Map.insert n val2 env')) (eval4 e)
      _ -> throwError "type error in application"

t4 :: (Either String Value, Integer)
t4 = runEval4 Map.empty 0 (eval4 exampleExp)

t4a :: (Either String Value, Integer)
t4a = runEval4 Map.empty 0 (eval4 exampleExpErr)

type Eval4' a = ReaderT Env (StateT Integer (ExceptT String Identity)) a

runEval4' :: Env -> Integer -> Eval4' a -> Either String (a, Integer)
runEval4' env st m = runIdentity (runExceptT (runStateT (runReaderT m env) st))

eval4' :: Expr -> Eval4' Value
eval4' (Lit i) =
  do
    tick
    return $ IntVal i
eval4' (Var n) =
  do
    tick
    env <- ask
    case Map.lookup n env of
      Nothing -> throwError $ "variable not found " ++ n
      Just a -> return a
eval4' (Plus e1 e2) =
  do
    tick
    val1 <- eval4' e1
    val2 <- eval4' e2
    case (val1, val2) of
      (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
      _ -> throwError "type error in addition"
eval4' (Abs n e) =
  do
    tick
    env <- ask
    return $ FunVal env n e
eval4' (App e1 e2) =
  do
    tick
    val1 <- eval4' e1
    val2 <- eval4' e2
    case val1 of
      FunVal env' n e -> withReaderT (const (Map.insert n val2 env')) (eval4' e)
      _ -> throwError "type error in application"

t4' :: Either String (Value, Integer)
t4' = runEval4' Map.empty 0 (eval4' exampleExp)

t4a' :: Either String (Value, Integer)
t4a' = runEval4' Map.empty 0 (eval4' exampleExpErr)

type Eval5 a =
  ReaderT
    Env
    ( ExceptT
        String
        ( WriterT
            [String]
            (StateT Integer Identity)
        )
    )
    a

runEval5 ::
  r ->
  s ->
  ReaderT r (ExceptT e (WriterT w (StateT s Identity))) a ->
  ((Either e a, w), s)
runEval5 env st m =
  runIdentity
    ( runStateT
        ( runWriterT
            ( runExceptT
                (runReaderT m env)
            )
        )
        st
    )

eval5 :: Expr -> Eval5 Value
eval5 (Lit i) =
  do
    tick
    return $ IntVal i
eval5 (Var n) =
  do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
      Nothing -> throwError $ "variable not found " ++ n
      Just a -> return a
eval5 (Plus e1 e2) =
  do
    tick
    val1 <- eval5 e1
    val2 <- eval5 e2
    case (val1, val2) of
      (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
      _ -> throwError "type error in addition"
eval5 (Abs n e) =
  do
    tick
    env <- ask
    return $ FunVal env n e
eval5 (App e1 e2) =
  do
    tick
    val1 <- eval5 e1
    val2 <- eval5 e2
    case val1 of
      FunVal env' n e -> withReaderT (const (Map.insert n val2 env')) (eval5 e)
      _ -> throwError "type error in application"

t5 :: ((Either String Value, [String]), Integer)
t5 = runEval5 Map.empty 0 (eval5 exampleExp)

t5a :: ((Either String Value, [String]), Integer)
t5a = runEval5 Map.empty 0 (eval5 exampleExpErr)

type Eval6 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) a

runEval6 ::
  r ->
  s ->
  ReaderT r (ExceptT e (WriterT w (StateT s m))) a ->
  m ((Either e a, w), s)
runEval6 env s m = runStateT (runWriterT (runExceptT (runReaderT m env))) s

eval6 :: Expr -> Eval6 Value
eval6 (Lit i) =
  do
    tick
    liftIO $ print i 
    return $ IntVal i
eval6 (Var n) =
  do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
      Nothing -> throwError $ "variable not found " ++ n
      Just a -> return a
eval6 (Plus e1 e2) =
  do
    tick
    val1 <- eval6 e1
    val2 <- eval6 e2
    case (val1, val2) of
      (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
      _ -> throwError "type error in addition"
eval6 (Abs n e) =
  do
    tick
    env <- ask
    return $ FunVal env n e
eval6 (App e1 e2) =
  do
    tick
    val1 <- eval6 e1
    val2 <- eval6 e2
    case val1 of
      FunVal env' n e -> withReaderT (const (Map.insert n val2 env')) (eval6 e)
      _ -> throwError "type error in application"

t6 :: IO ((Either String Value, [String]), Integer)
t6 = runEval6 Map.empty 0 (eval6 exampleExp)

t6a :: IO ((Either String Value, [String]), Integer)
t6a = runEval6 Map.empty 0 (eval6 exampleExpErr)