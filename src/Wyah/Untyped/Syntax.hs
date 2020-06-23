module Wyah.Untyped.Syntax
  ( Expr(..)
  , Lit(..)
  ) where

type Name = String

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
