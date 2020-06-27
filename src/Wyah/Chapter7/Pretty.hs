module Wyah.Chapter7.Pretty
  ( ppExpr
  , ppType
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, renderStrict)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), (<+>), parens, annotate, layoutSmart, defaultLayoutOptions)

import Wyah.Chapter7.Type (Type(..))
import Wyah.Chapter7.Syntax (Program(..), Decl(..), Expr(..), Lit(..), Var(..))
import qualified Wyah.Chapter7.Pretty.Style as Style
import Wyah.Chapter7.Pretty.Utils (parensIf)

-- | Pretty prints expression 'Expr'.
ppExpr :: Expr -> Text
ppExpr =
    renderStrict
  . layoutSmart defaultLayoutOptions
  . pp 0

-- | Pretty prints 'Type'.
ppType :: Type -> Text
ppType =
    renderStrict
  . layoutSmart defaultLayoutOptions
  . pretty

pp :: Int -> Expr -> Doc AnsiStyle
pp _ (EVar v) = annotate Style.var $ pretty v
pp d (EApp e1 e2) = parensIf (d > 0) $
      pp (d + 1) e1
  <+> pp d e2
pp d (ELam v e) = parensIf (d > 0) $
      annotate Style.lam (pretty '\\') <> pretty v
  <+> annotate Style.arrow "->"
  <+> pp (d + 1) e
pp d (ELet v e b) =
      annotate Style.letin "let" <+> pretty v
  <+> annotate Style.letin "="   <+> pp d e
  <+> annotate Style.letin "in"  <+> pp d b
pp _ (ELit l) = annotate Style.lit $ pretty l
pp d (EOp op x y) = parensIf (d > 0) $
      pp (d + 1) x
  <+> annotate Style.binOp (pretty op)
  <+> pp (d + 1) y
pp d (EIf c t f) =
      annotate Style.cond "if"   <+> pp d c
  <+> annotate Style.cond "then" <+> pp d t
  <+> annotate Style.cond "else" <+> pp d f
pp d (EFix e) = parensIf (d > 0) $
  annotate Style.fix "fix" <+> pp d e
