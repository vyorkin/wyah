module Wyah.Chapter7.Infer
  ( Infer
  , Unique(..)

  , runInfer

  , unify
  , instantiate
  , generalize

  , module Wyah.Chapter7.Infer.Subst
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad (replicateM)

import Wyah.Chapter7.Syntax (Expr(..), Var)
import Wyah.Chapter7.Type (Type(..), TVar(..), Scheme(..))
import qualified Wyah.Chapter7.Type as Type
import Wyah.Chapter7.TypeEnv (TypeEnv(..), typeOf)
import qualified Wyah.Chapter7.TypeEnv as TypeEnv

import Wyah.Chapter7.Infer.Types (Infer, TypeError(..), Unique(..), initUnique)
import Wyah.Chapter7.Infer.Subst (Subst, Substitutable(..), (|.|))
import qualified Wyah.Chapter7.Infer.Subst as Subst

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver = undefined

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unify :: Type -> Type -> Infer Subst
unify (a :-> b) (c :-> d) = do
  s1 <- unify a c
  s2 <- unify (apply s1 b) (apply s1 d)
  pure $ s2 |.| s1
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = pure Subst.empty
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = pure Subst.empty
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = pure $ Map.singleton a t

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- freshTypes as
  let s = Map.fromList $ zip as as'
  pure $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t where
  as = Set.toList $ ftv t `Set.difference` ftv env

freshTypes :: [TVar] -> Infer [Type]
freshTypes = mapM $ const freshType

freshType :: Infer Type
freshType = do
  s <- get
  put s { count = count s + 1 }
  pure $ TVar $ TV (letters !! count s)
  where
    letters :: [Text]
    letters = [1..]
          >>= flip replicateM ['a'..'z']
          >>= pure . Text.pack

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env = \case
  EVar v -> lookupEnv env v
  ELam v e -> do
    tv <- freshType
    let ts = Forall [] tv
    let env' = TypeEnv.extend v ts env
    (s1, t1) <- infer env' e
    pure (s1, apply s1 tv :-> t1)
  EApp e1 e2 -> do
    tv <- freshType
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) (t2 :-> tv)
    let tout = apply s3 tv
    pure $ (s3 |.| s2 |.| s1, tout)
  ELet v b e -> do
    (s1, t1) <- infer env b
    let env'  = apply s1 env
        t1'   = generalize env' t1
        env'' = TypeEnv.extend v t1' env'
    (s2, t2) <- infer env'' e
    pure (s1 |.| s2, t2)
  -- EOp op e1 e2 -> do



-- | Looks up the local variable reference in typing
-- environment and if found it instatiates a fresh copy.
lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) v = case Map.lookup v env of
  Nothing -> throwError $ UnboundVariable (show v)
  Just s  -> instantiate s >>= pure . (Subst.empty, )
