module Wyah.Chapter7.Syntax
  ( Program(..)
  , Decl(..)
  , Expr(..)
  , Lit(..)
  , Var(..)
  , BinOp(..)
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))

newtype Program = Program [Decl]
  deriving (Eq, Show)

data Decl = Decl Text Expr
  deriving (Eq, Show)

newtype Var = Var { unVar :: Text }
  deriving (Eq, Show)

instance Pretty Var where
  pretty (Var s) = pretty s

data Expr
  = EVar Var
  | EApp Expr Expr
  | ELam Var Expr
  | ELet Var Expr Expr
  | ELit Lit
  | EOp BinOp Expr Expr
  | EIf Expr Expr Expr
  | EFix Expr
  deriving (Eq, Show)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Show)

instance Pretty Lit where
  pretty (LInt n)  = pretty n
  pretty (LBool b) = pretty b

data BinOp = Add | Sub | Mul | Eq
  deriving (Eq, Ord, Show)

instance Pretty BinOp where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Eq  = "=="
