module Wyah.Chapter6.Lazy
  ( Thunk
  , Value(..)
  , update
  , force
  , mkThunk
  , eval
  , omega
  , test1
  , test2
  ) where

import Data.Text (Text)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Map ((!))
import Wyah.Chapter5.STLC.Env (Env)
import qualified Wyah.Chapter5.STLC.Env as Env

data Expr
  = EVar Text
  | ELam Text Expr
  | EApp Expr Expr
  | EBool Bool
  | EInt Int
  | EFix Expr
  deriving (Show)

data Value
  = VBool Bool
  | VInt Int
  | VClosure (Thunk -> IO Value)

instance Show Value where
  show (VBool b)    = show b
  show (VInt n)     = show n
  show (VClosure _) = "<<closure>>"

type VEnv = Env (IORef Thunk)

type Thunk = () -> IO Value

update :: IORef Thunk -> Value -> IO ()
update ref v = do
  writeIORef ref (\() -> pure v)
  pure ()

force :: IORef Thunk -> IO Value
force ref = do
  thunk <- readIORef ref
  value <- thunk ()
  update ref value
  pure value

mkThunk :: VEnv -> Text -> Expr -> (Thunk -> IO Value)
mkThunk env name body = \thunk -> do
  ref <- newIORef thunk
  eval (Env.extend name ref env) body

eval :: VEnv -> Expr -> IO Value
eval env = \case
  EVar n -> force (env ! n)
  ELam n e -> pure $ VClosure (mkThunk env n e)
  EApp e1 e2 -> do
    VClosure cl <- eval env e1
    cl (\() -> eval env e2)
  EBool b -> pure $ VBool b
  EInt n -> pure $ VInt n
  EFix e -> eval env (EApp e (EFix e))

-- omega = (\x -> x x) (\x -> x x)
-- test1 = (\y -> 42) omega

omega :: Expr
omega =
  EApp
  (ELam "x" (EApp (EVar "x") (EVar "x")))
  (ELam "x" (EApp (EVar "x") (EVar "x")))

-- test1 = (\y -> 42) omega
test1 :: IO Value
test1 = eval Env.empty $ EApp (ELam "y" (EInt 42)) omega

-- test2 = (\y -> 0) diverge
test2 :: IO Value
test2 = eval Env.empty $ EApp ignore diverge

-- diverge = fix (\x -> x x)
diverge :: Expr
diverge = EFix (ELam "x" (EApp (EVar "x") (EVar "x")))

-- ignore = \x -> 0
ignore :: Expr
ignore = ELam "x" (EInt 0)
