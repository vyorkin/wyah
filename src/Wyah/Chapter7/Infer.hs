module Wyah.Chapter7.Infer
  ( TypeEnv
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Wyah.Chapter7.Syntax (Var)
import Wyah.Chapter7.Type (Scheme)

type TypeEnv = Map Var Scheme
