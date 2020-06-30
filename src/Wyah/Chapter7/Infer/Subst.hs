module Wyah.Chapter7.Infer.Subst
  ( Subst
  , Substitutable(..)
  , empty
  , (|.|)
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Wyah.Chapter7.Type (Type(..), Scheme(..), TVar)
import Wyah.Chapter7.TypeEnv (TypeEnv(..))

-- Two operations that we'll perform quite a bit are
-- 1) querying free variables of an expression
-- 2) applying substitutions over expressions

-- | Substitution map.
type Subst = Map TVar Type

empty :: Subst
empty = Map.empty

(|.|) :: Subst -> Subst -> Subst
s1 |.| s2 = (apply s1 <$> s2) `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set TVar

instance Substitutable Type where
  apply _ t@(TCon _)  = t
  apply s t@(TVar a)  = Map.findWithDefault t a s
  apply s (t1 :-> t2) = apply s t1 :-> apply s t2

  ftv TCon{}      = Set.empty
  ftv (t1 :-> t2) = ftv t1 `Set.union` ftv t2
  ftv (TVar a)    = Set.singleton a

instance Substitutable Scheme where
  apply s (Forall as t) =
    let s' = foldr Map.delete s as
     in Forall as $ apply s' t

  ftv (Forall as t) =
    ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ apply s <$> env
  ftv (TypeEnv env) = ftv $ Map.elems env
