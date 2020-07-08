{-# LANGUAGE DerivingStrategies #-}

module Wyah.Chapter8.Error.SrcPos
  ( SrcPos(..)
  , fromCallStack
  ) where

import GHC.Stack (CallStack, getCallStack, SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))

-- | Human-readable info about the position in the source code.
newtype SrcPos = SrcPos String
  deriving newtype (Show, Eq)

-- | Display 'CallStack' as 'SrcPos' in a
-- format: @Module.function#line_number@.
fromCallStack :: CallStack -> SrcPos
fromCallStack cs = SrcPos ppCallStack where
    ppCallStack :: String
    ppCallStack = case getCallStack cs of
      [] -> "<unknown loc>"
      [(name, loc)] -> ppLoc name loc
      (_, loc) : (callerName, _) : _ -> ppLoc callerName loc

    ppLoc :: String -> SrcLoc -> String
    ppLoc name SrcLoc{..} =
      show srcLocModule
      <> "." <> show name
      <> "#" <> show srcLocStartLine
