module Wyah.Chapter7.Env
  ( Env
  , extend
  , empty
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

type Env k v = Map k v

extend :: Ord k => k -> a -> Env k a -> Env k a
extend = Map.insert

empty :: Env a
empty = Map.empty
