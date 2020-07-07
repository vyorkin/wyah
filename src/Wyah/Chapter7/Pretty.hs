module Wyah.Chapter7.Pretty
  ( renderRaw
  , renderAnn

  , render
  , layout

  , prettyProgram
  , prettyDecl
  , prettyTypeEnv
  , prettySignature
  , prettyScheme
  , prettyExpr'
  , prettyExpr
  , prettyValue
  , prettySteps
  , prettyStep
  , prettyInterpreterError
  ) where

import Data.Text (Text)
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal (renderStrict)
import Data.Text.Prettyprint.Doc
  (Doc, SimpleDocStream, Pretty(..), unAnnotate, vcat,
  (<+>), annotate, layoutSmart, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Wyah.Chapter7.Type (Type, Scheme)
import Wyah.Chapter7.TypeEnv (TypeEnv(..))
import Wyah.Chapter7.Syntax (Program(..), Decl(..), Expr(..), BinOp(..), Var(..), varName)
import Wyah.Chapter7.Eval (InterpreterError(..), Value(..), Step(..))
import qualified Wyah.Chapter7.Pretty.Style as Style
import Wyah.Chapter7.Pretty.Utils (parensIf)

renderRaw :: (a -> Doc AnsiStyle) -> a -> Text
renderRaw f = render renderStrict (unAnnotate . f)

renderAnn :: (a -> Doc AnsiStyle) -> a -> Text
renderAnn = render Terminal.renderStrict

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

prettySignature :: (Text, Scheme) -> Doc AnsiStyle
prettySignature (name, scheme) =
      annotate Style.var (pretty name)
  <+> annotate Style.colon ":"
  <+> prettyScheme scheme

prettyExpr' :: Expr -> Doc AnsiStyle
prettyExpr' = prettyExpr 0

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

prettyValue :: Value -> Doc AnsiStyle
prettyValue (VInt x)      = annotate Style.lit $ pretty x
prettyValue (VBool True)  = annotate Style.lit "true"
prettyValue (VBool False) = annotate Style.lit "false"
prettyValue (VClosure{})  = annotate Style.closure "<<closure>>"

prettyScheme :: Scheme -> Doc AnsiStyle
prettyScheme = annotate Style.ty . pretty

prettySteps :: [Step] -> Doc AnsiStyle
prettySteps = vcat . fmap prettyStep

prettyStep :: Step -> Doc AnsiStyle
prettyStep (e :>> v) = prettyExpr' e <+> "->" <+> prettyValue v

prettyTypeEnv :: TypeEnv -> Doc AnsiStyle
prettyTypeEnv (TypeEnv env) = vcat $
  prettySignature <$> (fmap sig $ Map.toList env)
  where
    sig (Var name, scheme) = (name, scheme)

prettyInterpreterError :: InterpreterError -> Doc AnsiStyle
prettyInterpreterError (NotInScope v) =
      "Variable"
  <+> annotate Style.lit (pretty v)
  <+> "is not in scope"
prettyInterpreterError (InvalidOperation op x y) =
      "Invalid operation"
  <+> prettyValue x
  <+> pretty op
  <+> prettyValue y
prettyInterpreterError (AppliedToNonClosure f arg) = vcat
  [ "Tried to apply to non-function type:"
  , prettyValue f
  , prettyValue arg
  ]
prettyInterpreterError (NonBooleanCondition cond) =
  "Non-boolean condition:" <+> prettyValue cond
