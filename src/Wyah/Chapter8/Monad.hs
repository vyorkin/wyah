{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wyah.Chapter8.Monad
  ( Compiler(..)
  , runCompiler
  , runCompilerIO
  ) where

import Data.Text (Text)
import Control.Monad.Reader (ReaderT(..), MonadReader, runReaderT)
import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (MonadError(..))
import Data.Either.Combinators (mapLeft)
import Control.Exception (catch, throwIO, try)

import Wyah.Chapter8.Error (CompilerError, CompilerException(..))
import qualified Wyah.Chapter8.Error as Error
import Wyah.Chapter8.Env (Env)
import qualified Wyah.Chapter8.Env as Env

newtype Compiler a = Compiler
  { unCompiler :: ReaderT Env IO a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

{- | This instance allows to throw and catch errors that are visible in type
definitions. The implementation relies on underlying 'IO' machinery.

Use 'Wyah.Chapter8.Error.throwError' and 'Wyah.Chapter8.Error.catchError'.
These functions automatically attach source code positions to errors.
-}
instance MonadError CompilerError Compiler where
  throwError :: CompilerError -> Compiler a
  throwError = liftIO . throwIO . CompilerException
  {-# INLINE throwError #-}

  catchError :: Compiler a -> (CompilerError -> Compiler a) -> Compiler a
  catchError action handler = Compiler $ ReaderT $ \env -> do
    let ioAction = runCompiler env action
    ioAction `catch` \(CompilerException e) -> runCompiler env $ handler e
  {-# INLINE catchError #-}

runCompiler :: Env -> Compiler a -> IO a
runCompiler env = flip runReaderT env . unCompiler

runCompilerIO :: Env -> Compiler a -> IO (Either CompilerError a)
runCompilerIO env c = mapLeft unCompilerException <$> try (runCompiler env c)
