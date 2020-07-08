module Wyah.Chapter8.Type
  ( Type(..)
  , Pred(..)
  , TVar(..)
  , TCon(..)
  , Alpha(..)
  ) where

import Data.String (IsString(..))
import Wyah.Chapter8.Name (Name)

data Type
  = TVar TVar
  | TCon TCon
  | Type :-> Type
  | Type :@: Type
  | TForall [Pred] [TVar] Type
  deriving (Eq, Read, Show)

data Pred = IsIn Name Type
  deriving (Eq, Read, Show)

infixr :->, :@:

newtype TVar = TV Name
  deriving (Eq, Ord, Read, Show)

instance IsString TVar where
  fromString = TV . fromString

data TCon
 = TConAlg Name
 | TConPrim Name
 deriving (Eq, Ord, Read, Show)

instance IsString TCon where
  fromString = TConAlg . fromString

-- Alpha equivalence

class Alpha a where
  aeq :: a -> a -> Bool

instance Alpha TVar where
  aeq _ _ = True

instance Alpha Type where
  aeq (TVar _) (TVar _)   = True
  aeq (a :@: b) (c :@: d) = aeq a c && aeq b d
  aeq (a :-> b) (c :-> d) = aeq a c && aeq b d
  aeq (TCon a) (TCon b)   = a == b
  aeq _ _                 = False
