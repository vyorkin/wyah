module Wyah.Chapter4.Pretty
  ( render
  , pp
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (
  Doc, Pretty(..), (<+>), sep, parens,
  layoutSmart, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Wyah.Chapter4.Syntax (Expr(..), Name(..), Lit(..))

-- | Pretty prints expression 'Expr'.
render :: Expr -> Text
render =
    renderStrict
  . layoutSmart defaultLayoutOptions
  . pp 0

pp :: Int -> Expr -> Doc acc
pp _ (EVar v) = pretty v
pp _ (ELit (LInt n)) = pretty n
pp _ (ELit (LBool b)) = pretty b
pp d e@(EApp _ _) = parensIf (d > 0) doc where
  doc = pp d f <+> sep (pp (d + 1) <$> xs)
  (f, xs) = viewApp e
pp d e@(ELam _ _) = parensIf (d > 0) doc where
  doc  = pretty '\\' <> sep vars <+> pretty '.' <+> body
  vars = pretty <$> viewVars e
  body = pp (d + 1) (viewBody e)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

viewVars :: Expr -> [Name]
viewVars (ELam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (ELam _ a) = viewBody a
viewBody x = x

viewApp :: Expr -> (Expr, [Expr])
viewApp (EApp e1 e2) = go e1 [e2]
  where
    go (EApp a b) xs = go a (b : xs)
    go f xs = (f, xs)
viewApp _ = error "Not application"
