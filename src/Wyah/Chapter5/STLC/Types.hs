module Wyah.Chapter5.STLC.Types
  ( VEnv
  , TEnv
  , Env
  , Value(..)
  ) where

import Data.Text (Text)

import Wyah.Chapter5.STLC.Env (Env)
import Wyah.Chapter5.STLC.Syntax (Expr, Type)

type VEnv = Env Value
type TEnv = Env Type

data Value
  = VInt Int
  | VBool Bool
  | VClosure Text Expr VEnv

instance Show Value where
  show (VInt x)   = show x
  show (VBool x)  = show x
  show VClosure{} = "<<closure>>"
