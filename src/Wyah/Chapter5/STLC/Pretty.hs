module Wyah.Chapter5.STLC.Pretty
  ( prettyExpr
  , prettyType
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, renderStrict)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), (<+>), parens, annotate, layoutSmart, defaultLayoutOptions)

import Wyah.Chapter5.STLC.Syntax (Expr(..), Lit(..), Type(..))
import qualified Wyah.Chapter5.STLC.Pretty.Style as Style

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

ppExpr :: Int -> Expr -> Doc AnsiStyle
ppExpr _ (EVar v) = annotate Style.var $ pretty v
ppExpr _ (ELit (LInt n)) = annotate Style.lit $ pretty n
ppExpr _ (ELit (LBool b)) = annotate Style.lit $ pretty b
ppExpr d (EApp e1 e2) = parensIf (d > 0) $
  ppExpr (d + 1) e1 <+> ppExpr d e2
ppExpr d (ELam (n, t) e) = parensIf (d > 0) $
      annotate Style.lam (pretty '\\') <> parens (pretty n <+> ty)
  <+> annotate Style.dot (pretty '.')
  <+> ppExpr (d + 1) e
  where
    ty =  annotate Style.colon (pretty ':')
      <+> annotate Style.ty (ppType d t)
ppExpr d (EOp op x y) = parensIf (d > 0) $
  ppExpr (d + 1) x <+> annotate Style.binOp (pretty op) <+> ppExpr (d + 1) y

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
