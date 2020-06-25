module Wyah.Chapter5.Calc.Check
  ( TypeError(..)
  , check
  , typeOf
  ) where

import Control.Monad ((<=<), when)
import Control.Monad.Except (Except, runExcept, throwError)

import Wyah.Chapter5.Calc.Syntax (Expr(..))
import Wyah.Chapter5.Calc.Type (Type(..))

type Check a = Except TypeError a

data TypeError = TypeMismatch !Type !Type
  deriving (Eq, Show)

check :: Expr -> Either TypeError Type
check = runExcept . typeOf

typeOf :: Expr -> Check Type
typeOf = \case
  Tr       -> pure TBool
  Fl       -> pure TBool
  Zero     -> pure TNat
  Succ e   -> assertExpr TNat TNat e
  Pred e   -> assertExpr TNat TNat e
  IsZero e -> assertExpr TNat TBool e
  If ce te fe -> do
    tce <- typeOf ce
    tte <- typeOf te
    tfe <- typeOf fe
    when (tce /= TBool) $ throwError (TypeMismatch tce TBool)
    when (tte /= tfe)   $ throwError (TypeMismatch tte tfe)
    pure tfe

assertTy
  :: Type -- ^ Expected type
  -> Type -- ^ Return type
  -> Type -- ^ Actual type
  -> Check Type
assertTy eTy rTy aTy
  | eTy == aTy = pure rTy
  | otherwise  = throwError $ TypeMismatch aTy eTy

assertExpr
  :: Type -- ^ Expected type
  -> Type -- ^ Return type
  -> Expr -- ^ Expression to assert
  -> Check Type
assertExpr eTy rTy = assertTy eTy rTy <=< typeOf
