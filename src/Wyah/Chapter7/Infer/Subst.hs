module Wyah.Chapter7.Infer.Subst
  ( Subst
  , Substitutable(..)
  , empty
  , compose
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Wyah.Chapter7.Type (Type(..), Scheme(..), TVar)
import Wyah.Chapter7.TypeEnv (TypeEnv(..))

type Subst = Map TVar Type

empty :: Subst
empty = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = apply s1 <$> s2 `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set TVar

instance Substitutable Type where
  apply _ t@(TCon _)  = t
  apply s t@(TVar a)  = Map.findWithDefault t a s
  apply s (t1 :-> t2) = apply s t1 :-> apply s t2

  ftv TCon{}      = Set.empty
  ftv (TVar a)    = Set.singleton a
  ftv (t1 :-> t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  -- forall a b c
  --   . (a -> c)
  --   -> d
  --   -> (e -> Bool)
  --   -> b

  -- s:
  -- a -> Bool
  -- c -> (Bool -> Int)
  -- d -> (Bool -> Bool)
  -- e -> Int

  -- s':
  -- d -> (Bool -> Bool)
  -- e -> Int

  -- forall a b c
  --   . (a -> c)
  --   -> (Bool -> Bool)
  --   -> (Int -> Bool)
  --   -> b
  apply s (Forall as t) =
    let s' = foldr Map.delete s as
     in Forall as $ apply s' t

  -- t = forall a b c d. (a -> b) -> (p -> q) -> c -> d
  -- ftv(t) = [p, q]
  ftv (Forall as t) =
    ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ apply s <$> env
  ftv (TypeEnv env) = ftv $ Map.elems env
