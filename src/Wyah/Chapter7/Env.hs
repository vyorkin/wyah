module Wyah.Chapter7.Env
  ( Env(..)
  , empty
  ) where

import qualified Data.Map as Map

import Wyah.Chapter7.Eval (TermEnv)
import Wyah.Chapter7.Infer (TypeEnv)

data Env = Env
  { typeEnv :: !TypeEnv -- ^ Type environment
  , termEnv :: !TermEnv -- ^ Term environment
  }

empty :: Env
empty = Env Map.empty Map.empty
