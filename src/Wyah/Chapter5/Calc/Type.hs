module Wyah.Chapter5.Calc.Type
  ( Type(..)
  ) where

import Data.Text.Prettyprint.Doc (Pretty(..))

data Type
  = TBool
  | TNat
  deriving (Eq, Show)

instance Pretty Type where
  pretty TBool = "Bool"
  pretty TNat  = "Nat"
