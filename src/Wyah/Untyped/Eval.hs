{-# LANGUAGE FlexibleContexts #-}
module Wyah.Untyped.Eval
  ( Value(..)
  , Scope
  , Step
  , Eval
  , EvalState(..)
  , runEval
  ) where

import Prelude hiding (log)

import Data.Text (Text)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.State (State, evalState, gets, modify)
import Control.Monad.Writer (WriterT, runWriterT, tell)

import Wyah.Untyped.Syntax (Expr(..), Lit(..), Name(..))

data Value
  = VInt Integer
  | VBool Bool
  | VClosure Text Expr Scope

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"

type Scope = Map Text Value

type Step = (Int, Expr)
type Eval a = WriterT [Step] (State EvalState) a

data EvalState = EvalState
  { depth :: Int
  } deriving (Show)

log :: Expr -> Eval ()
log e = do
  d <- gets depth
  tell [(d, e)]
  pure ()


inc :: Eval a -> Eval a
inc m = go 1 *> m <* go (-1) where
  go n = modify $ \s -> s { depth = depth s + n }

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)

eval :: Scope -> Expr -> Eval Value
eval scope expr = case expr of
  ELit (LInt x)   -> pure $ VInt (fromIntegral x)
  ELit (LBool x)  -> pure $ VBool x
  EVar (Name n)   -> log expr >> pure (scope ! n)
  ELam (Name n) e -> inc $ pure (VClosure n e scope)
  EApp e1 e2 -> inc do
    f <- eval scope e1
    log e1
    e <- eval scope e2
    log e2
    apply f e

apply :: Value -> Value -> Eval Value
apply (VClosure n e scope) v =
  let scope' = extend scope n v
   in eval scope' e
apply _ _ = error "Tried to apply non-closure"

extend :: Scope -> Text -> Value -> Scope
extend scope n v = Map.insert n v scope

emptyScope :: Scope
emptyScope = Map.empty
