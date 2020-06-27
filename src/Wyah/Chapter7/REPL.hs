{-# LANGUAGE FlexibleContexts #-}

module Wyah.Chapter7.REPL
  ( Repl
  , Env(..)

  , hoistErr

    -- * Parsing
  , parse

    -- * Execution
  , exec

    -- * Commands
  , browse
  , load
  , typeOf
  , quit

    -- * Main

  , run
  , main

  ) where

import Prelude hiding (init)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text.IO as Text (readFile)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExcept)
import Control.Monad.State (MonadState, StateT(..), evalStateT, get, gets)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)
import System.IO.Error (isEOFError, catchIOError)
import System.Console.Repline
  (HaskelineT(..), CompletionFunc, WordCompleter,
   CompleterStyle(..), abort, evalRepl, fileCompleter, wordCompleter)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Wyah.Chapter7.Syntax (Program, Expr, Var(..))
import Wyah.Chapter7.Eval (TermEnv)
import Wyah.Chapter7.Infer (TypeEnv)
import Wyah.Chapter7.Env (Env(..))
import qualified Wyah.Chapter7.Env as Env
import qualified Wyah.Chapter7.Lexer as Lexer
import qualified Wyah.Chapter7.Parser as Parser

type Repl a = HaskelineT (StateT Env IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right v)  = pure v
hoistErr (Left err) = liftIO (print err) >> abort

-- Parsing

parse :: String -> Either String Program
parse str = Lexer.runAlex str Parser.parse

-- Execution

cmd :: String -> Repl ()
cmd = exec True

exec :: Bool -> String -> Repl ()
exec update source = do
  env <- get
  program <- hoistErr $ parse source
  pure ()

-- Commands

browse :: [String] -> Repl ()
browse = undefined

load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ readFile (unwords args)
  exec True contents

typeOf :: [String] -> Repl ()
typeOf args = undefined

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- Main

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> run $ pure ()
    [file] -> run $ load [file]
    ["test", file] -> run $ load [file] >> browse [] >> quit ()
    _ -> putStrLn "invalid arguments"

run :: Repl a -> IO ()
run init = flip evalStateT Env.empty $
  evalRepl
    (pure "kek> ")
    cmd
    options
    (Just ':')
    completerStyle
    (welcome >> init)

welcome :: Repl ()
welcome = liftIO $ putStrLn "Hack & Kek!"

options :: [(String, [String] -> Repl ())]
options =
  [ ("load", load)
  , ("browse", browse)
  , ("quit", quit)
  , ("type", typeOf)
  ]

-- Tab completion

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load", fileCompleter)]

completer :: (Monad m, MonadState Env m) => WordCompleter m
completer n = do
  tenv <- gets typeEnv
  let defs = Text.unpack . unVar <$> Map.keys tenv
      cmds = [":load", ":browse", ":type", ":quit"]
      cmps = cmds ++ defs
  pure $ filter (List.isPrefixOf n) cmps

completerStyle :: CompleterStyle (StateT Env IO)
completerStyle = Prefix (wordCompleter completer) defaultMatcher