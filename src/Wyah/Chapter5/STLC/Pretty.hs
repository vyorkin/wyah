module Wyah.Chapter5.STLC.Pretty
  ( prettyExpr
  , prettyType
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), (<+>), parens, layoutSmart, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Wyah.Chapter5.STLC.Syntax (Expr(..), Lit(..), Type(..))

-- | Pretty prints expression 'Expr'.
prettyExpr :: Expr -> Text
prettyExpr =
    renderStrict
  . layoutSmart defaultLayoutOptions
  . ppExpr 0

-- | Pretty prints 'Type'.
prettyType :: Type -> Text
prettyType =
    renderStrict
  . layoutSmart defaultLayoutOptions
  . ppType 0

ppExpr :: Int -> Expr -> Doc acc
ppExpr _ (EVar v) = pretty v
ppExpr _ (ELit (LInt n)) = pretty n
ppExpr _ (ELit (LBool b)) = pretty b
ppExpr d (EApp e1 e2) = parensIf (d > 0) $
  ppExpr (d + 1) e1 <+> ppExpr d e2
ppExpr d (ELam (n, t) e) = parensIf (d > 0) $
      pretty '\\' <> parens (pretty n <+> ty)
  <+> pretty '.'
  <+> ppExpr (d + 1) e
  where ty = pretty ':' <+> ppType d t
ppExpr d (EOp op x y) = parensIf (d > 0) $
  ppExpr (d + 1) x <+> pretty op <+> ppExpr (d + 1) y

ppType :: Int -> Type -> Doc ann
ppType p = \case
  TInt  -> "Int"
  TBool -> "Bool"
  TArr a b ->
        parensIf (isArrow a) (ppType p a)
    <+> "->"
    <+> ppType p b

isArrow :: Type -> Bool
isArrow TArr{} = True
isArrow _ = False

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id
