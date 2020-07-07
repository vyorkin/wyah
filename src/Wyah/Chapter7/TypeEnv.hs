module Wyah.Chapter7.TypeEnv
  ( TypeEnv(..)
  , extend
  , empty
  , typeOf
  , vars
  ) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

import Wyah.Chapter7.Syntax (Var, varName)
import Wyah.Chapter7.Type (Scheme)

-- | Typing context (basically this is our "Gamma").
newtype TypeEnv = TypeEnv (Map Var Scheme)
  deriving (Semigroup)

empty :: TypeEnv
empty = TypeEnv Map.empty

extend :: Var -> Scheme -> TypeEnv -> TypeEnv
extend k v (TypeEnv env) = TypeEnv (Map.insert k v env)

typeOf :: TypeEnv -> Var -> Maybe Scheme
typeOf (TypeEnv env) k = Map.lookup k env

vars :: TypeEnv -> [Text]
vars (TypeEnv env) = varName <$> Map.keys env
