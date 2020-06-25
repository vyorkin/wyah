module Wyah.Chapter5.STLC.Syntax
  ( Expr(..)
  , Lit(..)
  , Name(..)
  , BinOp(..)
  , Type(..)
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
  | ELam (Name, Type) Expr
  | ELit Lit
  | EOp BinOp Expr Expr
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Ord, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Eq
  deriving (Eq, Ord, Show)

instance Pretty BinOp where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Eq  = "=="

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving (Eq, Read, Show)
