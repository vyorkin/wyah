module Wyah.Chapter7.Type
  ( Scheme(..)
  , Type(..)
  , TVar(..)

  , int
  , bool
  ) where

import Data.Text (Text)

data Type
  = TVar !TVar
  | TCon !Text
  | !Type :-> !Type
  deriving (Eq, Show)

infixr :->

newtype TVar = TV Text
  deriving (Eq, Show)

data Scheme = Forall [TVar] Type

int, bool :: Type
int  = TCon "Int"
bool = TCon "Bool"
