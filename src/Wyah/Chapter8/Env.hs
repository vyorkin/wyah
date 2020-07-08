module Wyah.Chapter8.Env
  ( Env(..)
  , empty
  ) where

import qualified Data.Set as Set
import qualified Data.Text.Lazy as Lazy (Text)

import Wyah.Chapter8.Frontend (Module)
import Wyah.Chapter8.Flags (Flags)

data Env = Env
  { filePath :: !(Maybe FilePath)  -- ^ File path
  , imports  :: ![FilePath]        -- ^ Loaded modules
  , src      :: !(Maybe Lazy.Text) -- ^ File source
  , ast      :: !(Maybe Module)    -- ^ AST
  , flags    :: !Flags             -- ^ Compiler flags
  } deriving (Eq, Read, Show)

empty :: Env
empty = Env
  { filePath = Nothing
  , imports = []
  , src = Nothing
  , ast = Nothing
  , flags = Set.empty
  }
