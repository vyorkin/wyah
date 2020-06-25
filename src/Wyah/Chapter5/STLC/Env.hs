module Wyah.Chapter5.STLC.Env
  ( Env
  , extend
  , empty
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

type Env a = Map Text a

extend :: Text -> a -> Env a -> Env a
extend = Map.insert

empty :: Env a
empty = Map.empty
