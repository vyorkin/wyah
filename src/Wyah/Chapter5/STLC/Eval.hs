module Wyah.Chapter5.STLC.Eval
  ( Eval
  , eval
  , runEval
  ) where

import Data.Map ((!))
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)

import Wyah.Chapter5.STLC.Syntax (Expr(..), Lit(..), BinOp(..), Name(..))
import Wyah.Chapter5.STLC.Types (VEnv, Value(..))
import qualified Wyah.Chapter5.STLC.Env as Env

type Eval a = ExceptT String (Writer [Expr]) a

runEval :: Expr -> (Either String Value, [Expr])
runEval expr = runWriter $ runExceptT (eval Env.empty expr)

eval :: VEnv -> Expr -> Eval Value
eval env expr = case expr of
  ELit (LInt x)  -> pure $ VInt x
  ELit (LBool x) -> pure $ VBool x
  EVar (Name n)  -> (lift $ tell [expr]) >> pure (env ! n)
  ELam (Name n, _) e -> pure (VClosure n e env)
  EApp e1 e2 -> do
    l <- eval env e1
    v <- eval env e2
    apply l v
  EOp op x y -> do
    a <- eval env x
    b <- eval env y
    binOp op a b

apply :: Value -> Value -> Eval Value
apply (VClosure n e env) v = eval (Env.extend n v env) e
apply _ _ = throwError "Tried to apply non-closure"

binOp :: BinOp -> Value -> Value -> Eval Value
binOp Add (VInt x) (VInt y)   = pure $ VInt (x + y)
binOp Sub (VInt x) (VInt y)   = pure $ VInt (x - y)
binOp Mul (VInt x) (VInt y)   = pure $ VInt (x * y)
binOp Eq  (VInt x) (VInt y)   = pure $ VBool (x == y)
binOp Eq  (VBool x) (VBool y) = pure $ VBool (x == y)
binOp op x y = throwError $
  "Invalid opration: " <> show x <> " " <> show op <> show y
