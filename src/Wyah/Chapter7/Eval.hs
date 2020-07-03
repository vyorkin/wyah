module Wyah.Chapter7.Eval
  ( Interpreter
  , InterpreterError(..)

  , TermEnv
  , Value(..)
  , Step(..)

  , emptyTermEnv

  , eval
  ) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)

import Wyah.Chapter7.Syntax (Expr(..), Lit(..), BinOp(..), Var(..))

data Step = Expr :>> Value

type TermEnv = Map Text Value

emptyTermEnv :: TermEnv
emptyTermEnv = Map.empty

data InterpreterError
  = NotInScope !Var
  | InvalidOperation !BinOp !Value !Value
  | AppliedToNonClosure !Value !Value
  | NonBooleanCondition !Value
  deriving (Show)

type Interpreter a = ExceptT InterpreterError (Writer [Step]) a

data Value
  = VInt !Integer
  | VBool !Bool
  | VClosure !Text !Expr !TermEnv

instance Show Value where
  show (VInt x)   = show x
  show (VBool x)  = show x
  show VClosure{} = "<<closure>>"

eval :: TermEnv -> Expr -> Interpreter Value
eval env e = case e of
  ELit (LInt n) ->
    ret e (VInt n)
  ELit (LBool b) ->
    ret e (VBool b)
  EVar var@(Var v) ->
    case Map.lookup v env of
      Just x  -> ret e x
      Nothing -> throwError $ NotInScope var
  EOp op x y -> do
    a <- eval env x
    b <- eval env y
    v <- binOp op a b
    ret e v
  ELam (Var v) body ->
    ret e $ VClosure v body env
  EApp e1 e2 -> do
    fun <- eval env e1
    arg <- eval env e2
    val <- apply fun arg
    ret e val
  ELet (Var v) expr body -> do
    expr' <- eval env expr
    let env' = Map.insert v expr' env
    val <- eval env' body
    ret e val
  EIf c t f -> do
    cond' <- eval env c
    val <- cond env cond' t f
    ret e val
  EFix expr -> do
    val <- eval env (EApp expr (EFix expr))
    ret e val

cond :: TermEnv -> Value -> Expr -> Expr -> Interpreter Value
cond env (VBool True)  t _ = eval env t
cond env (VBool False) _ t = eval env t
cond _ c _ _ = throwError $ NonBooleanCondition c

apply :: Value -> Value -> Interpreter Value
apply (VClosure v body env) arg = eval (Map.insert v arg env) body
apply v1 v2 = throwError $ AppliedToNonClosure v1 v2

ret :: Expr -> Value -> Interpreter Value
ret e v = tell [e :>> v] >> pure v

binOp :: BinOp -> Value -> Value -> Interpreter Value
binOp Add (VInt x)  (VInt y)  = pure $ VInt (x + y)
binOp Sub (VInt x)  (VInt y)  = pure $ VInt (x - y)
binOp Mul (VInt x)  (VInt y)  = pure $ VInt (x * y)
binOp Eq  (VInt x)  (VInt y)  = pure $ VBool (x == y)
binOp Eq  (VBool x) (VBool y) = pure $ VBool (x == y)
binOp op x y = throwError $ InvalidOperation op x y
