module Wyah.Chapter7.Ctx
  ( Ctx(..)
  , empty
  ) where

import qualified Data.Map as Map

import Wyah.Chapter7.Eval (TermEnv)
import Wyah.Chapter7.TypeEnv (TypeEnv)
import qualified Wyah.Chapter7.TypeEnv as TypeEnv

data Ctx = Ctx
  { typeEnv :: !TypeEnv -- ^ Type environment
  , termEnv :: !TermEnv -- ^ Term environment
  }

empty :: Ctx
empty = Ctx TypeEnv.empty Map.empty
