module Wyah.Chapter7.Syntax
  ( Expr(..)
  , Lit(..)
  , Var(..)
  , BinOp(..)
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))

type Decl = (Text, Expr)

data Program = Program [Decl] Expr
  deriving (Eq, Show)

newtype Var = Var Text
  deriving (Eq, Show)

instance Pretty Var where
  pretty (Var s) = pretty s

data Expr
  = EVar Var
  | EApp Expr Expr
  | ELam Var Expr
  | ELit Lit
  | EOp BinOp Expr Expr
  | EIf Expr Expr Expr
  | EFix Expr
  deriving (Eq, Show)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Show)

data BinOp = Add | Sub | Mul | Eq
  deriving (Eq, Ord, Show)

instance Pretty BinOp where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Eq  = "=="
