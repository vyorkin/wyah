module Wyah.Chapter7.Syntax
  ( Program(..)
  , Decl(..)
  , Expr(..)
  , Lit(..)
  , Var(..)
  , BinOp(..)
  , varName
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))

newtype Program = Program [Decl]
  deriving (Eq, Read, Show)

data Decl = Decl Text Expr
  deriving (Eq, Read, Show)

newtype Var = Var Text
  deriving (Eq, Ord, Read, Show)

varName :: Var -> Text
varName (Var x) = x

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
  deriving (Eq, Read, Show)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Read, Show)

instance Pretty Lit where
  pretty (LInt n)  = pretty n
  pretty (LBool True) = "true"
  pretty (LBool False) = "false"

data BinOp = Add | Sub | Mul | Eq
  deriving (Eq, Ord, Read, Show)

instance Pretty BinOp where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Eq  = "=="
