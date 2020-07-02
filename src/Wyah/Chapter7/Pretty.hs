module Wyah.Chapter7.Pretty
  ( ppProgram
  , ppProgramTerminal
  , ppDecl
  , ppDeclTerminal
  , ppExpr
  , ppExprTerminal
  , ppType
  , ppTypeTerminal
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal (renderStrict)
import Data.Text.Prettyprint.Doc
  (Doc, SimpleDocStream, Pretty(..), unAnnotate, vcat,
  (<+>), annotate, layoutSmart, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Wyah.Chapter7.Type (Type(..))
import Wyah.Chapter7.Syntax (Program(..), Decl(..), Expr(..), BinOp(..))
import qualified Wyah.Chapter7.Pretty.Style as Style
import Wyah.Chapter7.Pretty.Utils (parensIf)

ppProgram :: Program -> Text
ppProgram = render renderStrict (unAnnotate . prettyProgram)

ppProgramTerminal :: Program -> Text
ppProgramTerminal = render Terminal.renderStrict prettyProgram

ppDecl :: Decl -> Text
ppDecl = render renderStrict (unAnnotate . prettyDecl)

ppDeclTerminal :: Decl -> Text
ppDeclTerminal = render Terminal.renderStrict prettyDecl

-- | Pretty prints 'Expr'.
ppExpr :: Expr -> Text
ppExpr = render renderStrict (unAnnotate . prettyExpr 0)

-- | Pretty prints colorful 'Expr' for ANSI terminals.
ppExprTerminal :: Expr -> Text
ppExprTerminal = render Terminal.renderStrict (prettyExpr 0)

-- | Pretty prints 'Type'.
ppType :: Type -> Text
ppType = render renderStrict pretty

-- | Pretty prints colorful 'Type' for ANSI terminals.
ppTypeTerminal :: Type -> Text
ppTypeTerminal = render Terminal.renderStrict pretty

render
  :: (SimpleDocStream AnsiStyle -> a)
  -> (e -> Doc AnsiStyle)
  -> e -> a
render f pp = f . layout pp

layout :: (a -> Doc ann) -> a -> SimpleDocStream ann
layout f = layoutSmart defaultLayoutOptions . f

prettyProgram :: Program -> Doc AnsiStyle
prettyProgram (Program decls) = vcat (prettyDecl <$> decls)

prettyDecl :: Decl -> Doc AnsiStyle
prettyDecl (Decl name expr) =
      annotate Style.letin "let" <+> pretty name
  <+> annotate Style.letin "="   <+> prettyExpr 0 expr
  <> ";"

prettyExpr :: Int -> Expr -> Doc AnsiStyle
prettyExpr _ (EVar v) = annotate Style.var $ pretty v
prettyExpr d (EApp e1 e2) = parensIf (d > 0) $
      prettyExpr (d + 1) e1
  <+> prettyExpr (d + 1) e2
prettyExpr d (ELam v e) = parensIf (d > 0) $
      annotate Style.lam (pretty '\\') <> pretty v
  <+> annotate Style.arrow "->"
  <+> prettyExpr 0 e
prettyExpr d (ELet v b e) =
      annotate Style.letin "let" <+> pretty v
  <+> annotate Style.letin "="   <+> prettyExpr d b
  <+> annotate Style.letin "in"  <+> prettyExpr d e
prettyExpr _ (ELit l) = annotate Style.lit $ pretty l
prettyExpr d (EOp Eq x y) = parensIf (d > 0) $
      prettyExpr d x
  <+> annotate Style.binOp (pretty Eq)
  <+> prettyExpr d y
prettyExpr d (EOp op x y) = parensIf (d > 0) $
      prettyExpr (d + 1) x
  <+> annotate Style.binOp (pretty op)
  <+> prettyExpr (d + 1) y
prettyExpr d (EIf c t f) =
      annotate Style.cond "if"   <+> prettyExpr d c
  <+> annotate Style.cond "then" <+> prettyExpr d t
  <+> annotate Style.cond "else" <+> prettyExpr d f
prettyExpr d (EFix e) = parensIf (d > 0) $
  annotate Style.fix "fix" <+> prettyExpr (d + 1) e
