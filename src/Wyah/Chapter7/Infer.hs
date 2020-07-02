module Wyah.Chapter7.Infer
  ( Infer
  , Unique(..)

  , inferTop'
  , inferTop

  , inferDecl'
  , inferDecl

  , inferExpr'
  , inferExpr

  , infer
  , inferPrim
  , inferStep

  , unify
  , instantiate
  , generalize

  , module Wyah.Chapter7.Infer.Subst
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (evalState, get, put)
import Control.Monad (replicateM, foldM)

import Wyah.Chapter7.Syntax (Decl(..), Expr(..), Var(..), BinOp(..), Lit(..))
import Wyah.Chapter7.Type (Type(..), TVar(..), Scheme(..))
import qualified Wyah.Chapter7.Type as Type
import Wyah.Chapter7.TypeEnv (TypeEnv(..))
import qualified Wyah.Chapter7.TypeEnv as TypeEnv

import Wyah.Chapter7.Infer.Types (Infer, TypeError(..), Unique(..), initUnique)
import Wyah.Chapter7.Infer.Subst (Subst, Substitutable(..), (|.|))
import qualified Wyah.Chapter7.Infer.Subst as Subst

inferTop' :: [(Var, Expr)] -> Either TypeError TypeEnv
inferTop' = inferTop TypeEnv.empty

inferTop :: TypeEnv -> [(Var, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((var, expr):decls) =
  case inferExpr env expr of
    Left err -> Left err
    Right tv -> inferTop (TypeEnv.extend var tv env) decls

inferDecl' :: Decl -> Either TypeError Scheme
inferDecl' = inferDecl TypeEnv.empty

inferDecl :: TypeEnv -> Decl -> Either TypeError Scheme
inferDecl env (Decl _ expr) = inferExpr env expr

inferExpr' :: Expr -> Either TypeError Scheme
inferExpr' = inferExpr TypeEnv.empty

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Subst, Type) -> Scheme
closeOver (s, t) = normalize sc where
  sc = generalize TypeEnv.empty (apply s t)

-- | Normalization is just a renaming of
-- generalized variables in a type scheme (polymorphic type).
normalize :: Scheme -> Scheme
normalize (Forall _ t) =
  let as = snd <$> freshFvs
      t' = norm t
   in Forall as t'
  where
    freshFvs :: [(TVar, TVar)]
    freshFvs = zip (Set.toList $ ftv t) (TV <$> typeLetters)

    norm :: Type -> Type
    norm (a :-> b) = norm a :-> norm b
    norm (TCon a)  = TCon a
    norm (TVar v)  = case lookup v freshFvs of
      Just x  -> TVar x
      Nothing -> error "Type variable not in signature"

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

-- λ> sc = Forall [TV "a", TV "b"] (TVar (TV "a") :-> TVar (TV "b"))
-- λ> evalState (runExceptT (instantiate sc)) initUnique
-- Right (TVar (TV "a") :-> TVar (TV "b"))
-- λ> = evalState (runExceptT (instantiate sc)) (Unique 2)
-- Right (TVar (TV "c") :-> TVar (TV "d"))

-- | Instantiation is a converting a type scheme (polymorphic type) into
-- a regular type by creating fresh names for each type variable that
-- doesn't occur in the current type enviroment.
instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- freshTypes as
  let s = Map.fromList $ zip as as'
  pure $ apply s t

-- | Generalization: converting a type by closing
-- over all free type variables in a type scheme.
-- See README.md for example.
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t where
  as = Set.toList $ ftv t `Set.difference` ftv env

freshTypes :: [TVar] -> Infer [Type]
freshTypes = mapM $ const freshType

freshType :: Infer Type
freshType = do
  s <- get
  put s { count = count s + 1 }
  pure $ TVar $ TV (typeLetters !! count s)

typeLetters :: [Text]
typeLetters =
      [1..]
  >>= flip replicateM ['a'..'z']
  >>= pure . Text.pack

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env = \case
  EVar v ->
    lookupEnv env v
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
  EOp op e1 e2 ->
    inferPrim env [e1, e2] (binOpType op)
  EIf c t f -> do
    tv <- freshType
    inferPrim env [c, t, f] (Type.bool :-> tv :-> tv :-> tv)
  EFix e -> do
    tv <- freshType
    inferPrim env [e] ((tv :-> tv) :-> tv)
  ELit (LInt _)  -> pure (Subst.empty, Type.int)
  ELit (LBool _) -> pure (Subst.empty, Type.bool)

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env es t = do
  tv <- freshType
  -- We're building an arrow type here where
  -- - 'Subst.empty' is initial set of substitutions
  -- - 'id' is the "last" continuation which does nothing
  (s1, tf) <- foldM (inferStep env) (Subst.empty, id) es
  -- 'tf' is a function which given a type of
  -- result will build a whole arrow type (ending with that type)
  let t' = apply s1 (tf tv)
  s2 <- unify t' t
  pure (s2 |.| s1, apply s2 tv)

inferStep
  :: TypeEnv
  -> (Subst, Type -> a)
  -> Expr
  -> Infer (Subst, Type -> a)
inferStep env (s, tf) e = do
  let env' = apply s env
  (s', t) <- infer env' e
  let cont t' = tf $ t :-> t'
  pure (s' |.| s, cont)

binOpType :: BinOp -> Type
binOpType = \case
  Add -> Type.int :-> Type.int :-> Type.int
  Sub -> Type.int :-> Type.int :-> Type.int
  Mul -> Type.int :-> Type.int :-> Type.int
  Eq  -> Type.int :-> Type.int :-> Type.bool

-- | Looks up the local variable reference in typing
-- environment and if found it instatiates a fresh copy.
lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) v = case Map.lookup v env of
  Nothing -> throwError $ UnboundVariable (show v)
  Just s  -> instantiate s >>= pure . (Subst.empty, )
