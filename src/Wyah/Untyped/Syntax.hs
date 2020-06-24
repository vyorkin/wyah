module Wyah.Untyped.Syntax
  ( Expr(..)
  , Lit(..)
  , Name(..)
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))

newtype Name = Name Text
  deriving (Eq, Show)

instance Pretty Name where
  pretty (Name s) = pretty s

data Expr
  = EVar Name
  | EApp Expr Expr
  | ELam Name Expr
  | ELit Lit
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Ord, Show)
