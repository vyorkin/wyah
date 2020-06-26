module Wyah.Chapter7.Eval
  ( TermEnv
  , Value(..)
  ) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

import Wyah.Chapter7.Syntax (Expr)

type TermEnv = Map Text Value

data Value
  = VInt !Integer
  | VBool !Bool
  | VClosure !Text !Expr !TermEnv
