module Wyah.Chapter7.Type
  ( Scheme(..)
  , Type(..)
  , TVar(..)

  , isArrow

  , int
  , bool
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..), (<+>), hcat, punctuate)

import Wyah.Chapter7.Pretty.Utils (parensIf)

-- | System F type ("strong polymorphism").
data Type
  = TVar !TVar       -- ^ Universal type.
  | TCon !Text       -- ^ Simple (builtin) type (constructor).
  | !Type :-> !Type  -- ^ Arrow type.
  deriving (Eq, Read, Show)

infixr :->

instance Pretty Type where
  pretty (TVar v) = pretty v
  pretty (TCon c) = pretty c
  pretty (a :-> b) =
    parensIf (isArrow a) (pretty a)
    <+> "->" <+> pretty b

newtype TVar = TV Text
  deriving (Eq, Ord, Read, Show)

instance Pretty TVar where
  pretty (TV v) = pretty v

data Scheme = Forall [TVar] Type

instance Pretty Scheme where
  pretty (Forall [] t) = pretty t
  pretty (Forall ts t) =
        "forall"
    <+> hcat (punctuate " " $ pretty <$> ts)
     <> pretty '.'
    <+> pretty t

isArrow :: Type -> Bool
isArrow (_ :-> _) = True
isArrow _ = False

int, bool :: Type
int  = TCon "Int"
bool = TCon "Bool"
