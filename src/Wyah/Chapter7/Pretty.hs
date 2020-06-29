module Wyah.Chapter7.Pretty
  ( ppExpr
  , ppExprTerminal
  , ppType
  , ppTypeTerminal
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal (renderStrict)
import Data.Text.Prettyprint.Doc
  (Doc, SimpleDocStream, Pretty(..),
  (<+>), annotate, layoutSmart, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Wyah.Chapter7.Type (Type(..))
import Wyah.Chapter7.Syntax (Expr(..), BinOp(..))
import qualified Wyah.Chapter7.Pretty.Style as Style
import Wyah.Chapter7.Pretty.Utils (parensIf)

-- | Pretty prints 'Expr'.
ppExpr :: Expr -> Text
ppExpr = renderStrict . layout (pp 0)

-- | Pretty prints colorful 'Expr' for ANSI terminals.
ppExprTerminal :: Expr -> Text
ppExprTerminal = Terminal.renderStrict . layout (ppTerminal 0)

-- | Pretty prints 'Type'.
ppType :: Type -> Text
ppType = renderStrict . layout pretty

-- | Pretty prints colorful 'Type' for ANSI terminals.
ppTypeTerminal :: Type -> Text
ppTypeTerminal = Terminal.renderStrict . layout pretty

layout :: (a -> Doc ann) -> a -> SimpleDocStream ann
layout f = layoutSmart defaultLayoutOptions . f

ppTerminal :: Int -> Expr -> Doc AnsiStyle
ppTerminal _ (EVar v) = annotate Style.var $ pretty v
ppTerminal d (EApp e1 e2) = parensIf (d > 0) $
      ppTerminal (d + 1) e1
  <+> ppTerminal (d + 1) e2
ppTerminal d (ELam v e) = parensIf (d > 0) $
      annotate Style.lam (pretty '\\') <> pretty v
  <+> annotate Style.arrow "->"
  <+> ppTerminal 0 e
ppTerminal d (ELet v b e) =
      annotate Style.letin "let" <+> pretty v
  <+> annotate Style.letin "="   <+> ppTerminal d b
  <+> annotate Style.letin "in"  <+> ppTerminal d e
ppTerminal _ (ELit l) = annotate Style.lit $ pretty l
ppTerminal d (EOp Eq x y) = parensIf (d > 0) $
      ppTerminal d x
  <+> annotate Style.binOp (pretty Eq)
  <+> ppTerminal d y
ppTerminal d (EOp op x y) = parensIf (d > 0) $
      ppTerminal (d + 1) x
  <+> annotate Style.binOp (pretty op)
  <+> ppTerminal (d + 1) y
ppTerminal d (EIf c t f) =
      annotate Style.cond "if"   <+> ppTerminal d c
  <+> annotate Style.cond "then" <+> ppTerminal d t
  <+> annotate Style.cond "else" <+> ppTerminal d f
ppTerminal d (EFix e) = parensIf (d > 0) $
  annotate Style.fix "fix" <+> ppTerminal (d + 1) e

pp :: Int -> Expr -> Doc ann
pp _ (EVar v) = pretty v
pp d (EApp e1 e2) = parensIf (d > 0) $
      pp (d + 1) e1
  <+> pp (d + 1) e2
pp d (ELam v e) = parensIf (d > 0) $
      pretty '\\' <> pretty v
  <+> "->"
  <+> pp 0 e
pp d (ELet v e b) =
      "let" <+> pretty v
  <+> "="   <+> pp d e
  <+> "in"  <+> pp d b
pp _ (ELit l) = pretty l
pp d (EOp Eq x y) = parensIf (d > 0) $
      pp d x
  <+> pretty Eq
  <+> pp d y
pp d (EOp op x y) = parensIf (d > 0) $
      pp (d + 1) x
  <+> pretty op
  <+> pp (d + 1) y
pp d (EIf c t f) =
      "if"   <+> pp d c
  <+> "then" <+> pp d t
  <+> "else" <+> pp d f
pp d (EFix e) = parensIf (d > 0) $
  "fix" <+> pp (d + 1) e
