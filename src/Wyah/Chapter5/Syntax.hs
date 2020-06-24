module Wyah.Chapter5.Syntax
  ( Expr(..)
  ) where

data Expr
  = Tr
  | Fl
  | Zero
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr Expr Expr
  deriving (Eq, Show)
