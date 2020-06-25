module Wyah.Chapter5.Calc.Pretty
  ( renderExpr
  , renderType
  , ppExpr
  , ppType
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (
  Doc, Pretty(..), (<+>), parens,
  layoutSmart, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Wyah.Chapter5.Calc.Syntax (Expr(..))
import Wyah.Chapter5.Calc.Type (Type(..))

renderType :: Type -> Text
renderType =
    renderStrict
  . layoutSmart defaultLayoutOptions
  . ppType

-- | Pretty prints expression 'Expr'.
renderExpr :: Expr -> Text
renderExpr =
    renderStrict
  . layoutSmart defaultLayoutOptions
  . ppExpr 0

ppType :: Type -> Doc ann
ppType = pretty

ppExpr :: Int -> Expr -> Doc ann
ppExpr _ Zero = pretty (0 :: Int)
ppExpr _ Tr = "true"
ppExpr _ Fl = "false"
ppExpr d (Succ e) = ppCtor "succ" d (ppExpr (d + 1) e)
ppExpr d (Pred e) = ppCtor "pred" d (ppExpr (d + 1) e)
ppExpr d (IsZero e) = ppCtor "iszero" d (ppExpr (d + 1) e)
ppExpr d (If ce te fe) =
      "if"   <+> ppExpr d ce
  <+> "then" <+> ppExpr d te
  <+> "else" <+> ppExpr d fe

ppCtor :: Doc ann -> Int -> Doc ann -> Doc ann
ppCtor s d e = parensIf (d > 0) (s <+> e)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id
