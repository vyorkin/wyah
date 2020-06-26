module Wyah.Chapter5.STLC.Eval
  ( Eval
  , Step(..)
  , eval
  , runEval
  ) where

import Data.Map ((!))
import Data.Text (Text)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)

import Wyah.Chapter5.STLC.Syntax (Expr(..), Lit(..), BinOp(..), Name(..))
import Wyah.Chapter5.STLC.Types (VEnv, Value(..))
import Wyah.Chapter5.STLC.Pretty (prettyExpr)
import qualified Wyah.Chapter5.STLC.Env as Env

data Step = Text :-> Value

type Eval a = ExceptT String (Writer [Step]) a

runEval :: Expr -> (Either String Value, [Step])
runEval = runWriter . runExceptT . evalLog Env.empty

eval :: VEnv -> Expr -> Eval Value
eval env expr = case expr of
  ELit (LInt x)  -> pure $ VInt x
  ELit (LBool x) -> pure $ VBool x
  EVar (Name n)  -> pure (env ! n)
  ELam (Name n, _) e -> pure (VClosure n e env)
  EApp e1 e2 -> do
    l <- eval env e1
    v <- eval env e2
    apply l v
  EOp op x y -> do
    a <- eval env x
    b <- eval env y
    v <- binOp op a b
    tell [prettyExpr expr :-> v]
    pure v

evalLog :: VEnv -> Expr -> Eval Value
evalLog env expr = do
  v <- eval env expr
  lift $ tell [prettyExpr expr :-> v]
  pure v

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
