module Wyah.Chapter7.Infer
  ( Infer
  , Unique(..)
  , TypeError(..)

  , runInfer

  , module Wyah.Chapter7.Infer.Subst
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState)

import Wyah.Chapter7.Syntax (Var)
import Wyah.Chapter7.Type (Type, TVar, Scheme(..))
import Wyah.Chapter7.Infer.Subst (Subst, Substitutable(..))
import Wyah.Chapter7.TypeEnv (TypeEnv, typeOf)
import qualified Wyah.Chapter7.TypeEnv as TypeEnv

type Infer a = ExceptT TypeError (State Unique) a

type TypeEnv = Map Var Scheme

data TypeError
  = UnificationFail !Type !Type
  | InfiniteType !TVar !Type
  | UnboundVariable !String

newtype Unique = Unique { count :: Int }

initUnique :: Unique
initUnique = Unique { count = 0 }

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver = undefined
