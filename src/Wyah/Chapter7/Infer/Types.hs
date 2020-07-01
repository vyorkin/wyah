module Wyah.Chapter7.Infer.Types
  ( Infer
  , TypeError(..)
  , Unique(..)
  , initUnique
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Wyah.Chapter7.Type (Type(..), TVar(..))

-- | Monad that allows error reporting and
-- keeps track of a counter which we use to generate fresh variable names.
type Infer a = ExceptT TypeError (State Unique) a

data TypeError
  = UnificationFail !Type !Type
  | InfiniteType !TVar !Type
  | UnboundVariable !String
  deriving (Show)

newtype Unique = Unique { count :: Int }

initUnique :: Unique
initUnique = Unique { count = 0 }
