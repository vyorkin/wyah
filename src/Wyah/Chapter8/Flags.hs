module Wyah.Chapter8.Flags
  ( Flags
  , Flag(..)
  ) where

import Data.Set (Set)

-- | Set of compiler flags.
type Flags = Set Flag

-- | Compiler flag.
data Flag
  = DumpC
  | DumpLLVM    -- ^ \-ddump-llvm
  | DumpASM     -- ^ \-ddump-asm
  | DumpParsed  -- ^ \-ddump-parsed
  | DumpDesugar -- ^ \-ddump-desugar
  | DumpInfer   -- ^ \-ddump-infer
  | DumpCore    -- ^ \-ddump-core
  | DumpTypes   -- ^ \-ddump-types
  | DumpKinds   -- ^ \-ddump-types
  | DumpStg     -- ^ \-ddump-stg
  | DumpImp     -- ^ \-ddump-imp
  | DumpRenamer -- ^ \-ddump-rn
  | DumpToFile  -- ^ \-ddump-to-file
  deriving (Eq, Ord, Read, Show)
