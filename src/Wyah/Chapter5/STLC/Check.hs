module Wyah.Chapter5.STLC.Check
  ( Check
  , TypeError
  , runCheck
  , checkTop
  , check
  , errorText
  ) where

import Data.Text (Text)
import Control.Monad (when)
import Control.Monad.Reader (Reader, runReader, ask, local)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.Map as Map

import Wyah.Chapter5.STLC.Syntax (Expr(..), Name(..), Lit(..), Type(..))
import Wyah.Chapter5.STLC.Types (TEnv)
import qualified Wyah.Chapter5.STLC.Env as Env
import Wyah.Chapter5.STLC.Pretty (prettyExpr, prettyType)

data TypeError
  = Mismatch !Type !Type
  | NotFunction !(Expr, Type) !(Expr, Type)
  | NotInScope !Name
  deriving (Eq)

errorText :: TypeError -> Text
errorText (Mismatch a b) =
  "Expecting " <> prettyType b <> " but got " <> prettyType a
errorText (NotFunction (e1, t1) (e2, t2)) =
     "Tried to apply to non-function type:"
  <> "\nLambda: "
  <> prettyExpr e1
  <> " : "
  <> prettyType t1
  <> "\nArgument: "
  <> prettyExpr e2
  <> " : "
  <> prettyType t2
errorText (NotInScope (Name n)) =
  "Variable " <> n <> " is not in scope"

type Check a = ExceptT TypeError (Reader TEnv) a

runCheck :: TEnv -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: TEnv -> Expr -> Either TypeError Type
checkTop env = runCheck env . check

check :: Expr -> Check Type
check = \case
  ELit LInt{}  -> pure TInt
  ELit LBool{} -> pure TBool
  ELam (n, t) e -> do
    r <- inEnv n t (check e)
    pure $ TArr t r
  EApp e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    assertApp (e1, t1) (e2, t2)
  EOp _ e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    when (t1 /= t2) $ throwError (Mismatch t1 t2)
    pure t1
  EVar n -> lookupVar n

-- EApp
--   (EApp
--     ( ELam (Name "x", TInt)
--       (ELam (Name "y", TInt) (EVar (Name "y")))
--     )
--     (ELit (LInt 1)))
--   (ELit (LInt 2))

assertApp :: (Expr, Type) -> (Expr, Type) -> Check Type
assertApp (_, (TArr t1 _)) (_, t2)
  | t1 == t2  = pure t2
  | otherwise = throwError $ Mismatch t2 t1
assertApp et1 et2 = throwError $ NotFunction et1 et2

inEnv :: Name -> Type -> Check a -> Check a
inEnv (Name n) = local . Env.extend n

lookupVar :: Name -> Check Type
lookupVar n@(Name var) = do
  env <- ask
  case Map.lookup var env of
    Just e  -> pure e
    Nothing -> throwError $ NotInScope n
