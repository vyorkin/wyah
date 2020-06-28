module Wyah.Chapter7.TypeEnv
  ( TypeEnv(..)
  , extend
  , empty
  , typeOf
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Wyah.Chapter7.Syntax (Var)
import Wyah.Chapter7.Type (Scheme)

newtype TypeEnv = TypeEnv (Map Var Scheme)

empty :: TypeEnv
empty = TypeEnv Map.empty

extend :: Var -> Scheme -> TypeEnv -> TypeEnv
extend k v (TypeEnv env) = TypeEnv (Map.insert k v env)

typeOf :: TypeEnv -> Var -> Maybe Scheme
typeOf (TypeEnv env) k = Map.lookup k env
