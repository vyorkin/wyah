{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Wyah.Chapter8.Error
  ( CompilerError
  , CompilerErrorType(..)
  , TypeError
  , CompilerException(..)
    -- * Constraint synonyms
  , WithCompilerError
  , WithError
    -- * Common functions
  , throwError
  , catchError
  , liftErrorWith
  , liftError
    -- * Helpers
  , toNoSourceException
    -- ** Throwing helpers
  , throwOnNothing
  , throwOnNothingM
    -- * Re-exports
  , SrcPos
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as Except (throwError, catchError)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

import Wyah.Chapter8.Error.SrcPos (SrcPos(..))
import qualified Wyah.Chapter8.Error.SrcPos as SrcPos
import Wyah.Chapter8.Type (Type)

-- | Type alias for our application errors.
type WithCompilerError m = WithError CompilerErrorType m

-- | Type alias for general errors that has access to 'CallStack'.
type WithError err m = (MonadError (ErrorWithSource err) m, HasCallStack)

-- | Specialized version of 'Except.throwError'
throwError :: WithError err m => err -> m a
throwError = Except.throwError . ErrorWithSource (SrcPos.fromCallStack callStack)
{-# INLINE throwError #-}

-- | Specialized version of 'E.catchError'.
catchError :: WithError err m => m a -> (err -> m a) -> m a
catchError action handler = action `Except.catchError` (handler . errType)
{-# INLINE catchError #-}

-- | Lift errors from 'Either' by rethrowing them with attached source position.
liftErrorWith :: WithError e' m => (e -> e') -> Either e a -> m a
liftErrorWith f = either (throwError . f) pure
{-# INLINE liftErrorWith #-}

-- | Same as `liftErrorWith', but without mapping the error type.
liftError :: WithError e m => Either e a -> m a
liftError = liftErrorWith id
{-# INLINE liftError #-}


-- | Specialized application error with
-- attached source code position.
type CompilerError = ErrorWithSource CompilerErrorType

data CompilerErrorType
  = TypeError TypeError
  deriving (Eq, Show)

data TypeError
  = TypeMismatch Type Type
  deriving (Eq, Show)

newtype CompilerException err = CompilerException
  { unCompilerException :: ErrorWithSource err
  } deriving stock (Show)
    deriving anyclass (Exception)

-- | Wrapper around error type with attached source code position.
data ErrorWithSource err = ErrorWithSource
  { errSrcPos :: !SrcPos
  , errType   :: !err
  } deriving (Show, Eq, Functor)

-- Helpers

-- | Helper to convert @err@ into something that can be thrown
-- when you don't have the ability to specify the 'SrcPos'.
toNoSourceException :: err -> CompilerException err
toNoSourceException = CompilerException . ErrorWithSource (SrcPos "<unknown loc>")
{-# INLINE toNoSourceException #-}

-- | Extract the value from a maybe, throwing the given @err@ if the value does not exist.
throwOnNothing :: WithError err m => err -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure
{-# INLINE throwOnNothing #-}

-- | Extract the value from a 'Maybe' in @m@, throwing the given @err@ if the value does not exist.
throwOnNothingM :: WithError err m => err -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ action >>= throwOnNothing err
{-# INLINE throwOnNothingM #-}
