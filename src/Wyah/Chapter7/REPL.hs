{-# LANGUAGE FlexibleContexts #-}

module Wyah.Chapter7.REPL
  ( Repl

  , hoistErr

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
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.List as List
import qualified Data.Text.IO as Text (readFile)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExcept)
import Control.Monad.State (MonadState, StateT(..), evalStateT, get, put, gets)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)
import System.IO.Error (isEOFError, catchIOError)
import System.Console.Repline
  (HaskelineT(..), CompletionFunc, WordCompleter,
   CompleterStyle(..), abort, evalRepl, fileCompleter, wordCompleter)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Data.Text.Prettyprint.Doc (Doc, vcat)

import Wyah.Chapter7.Syntax (Program(..), Decl(..), Expr, Var(..), varName)
import Wyah.Chapter7.Eval (TermEnv, Value, InterpreterError, runEval', runEval)
import Wyah.Chapter7.Ctx (Ctx(..))
import Wyah.Chapter7.Infer (inferDecls)
import qualified Wyah.Chapter7.Infer as Infer
import Wyah.Chapter7.Type (Scheme)
import Wyah.Chapter7.TypeEnv (TypeEnv)
import qualified Wyah.Chapter7.TypeEnv as TypeEnv
import qualified Wyah.Chapter7.Ctx as Ctx
import qualified Wyah.Chapter7.Lexer as Lexer
import Wyah.Chapter7.Parser (parseProgram', parseExpr')
import qualified Wyah.Chapter7.Parser as Parser
import Wyah.Chapter7.Pretty
   (render, renderAnn, prettySignature,
    prettyValue, prettyTypeEnv, prettyInterpreterError)

type Repl a = HaskelineT (StateT Ctx IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right v)  = pure v
hoistErr (Left err) = liftIO (print err) >> abort

cmd :: String -> Repl ()
cmd = exec True

evalDecl :: TermEnv -> Decl -> TermEnv
evalDecl tenv (Decl name expr) = tenv' where
  (_, tenv', _) = runEval tenv name expr

exec :: Bool -> String -> Repl ()
exec update source = do
  Ctx{..} <- get
  Program decls <- hoistErr $ parseProgram' source
  typeEnv' <- hoistErr $ inferDecls typeEnv decls
  let termEnv' = foldl' evalDecl termEnv decls
      ctx = Ctx { typeEnv = typeEnv <> typeEnv'
                , termEnv = termEnv'
                }
  when update $ put ctx
  case List.find isIt decls of
    Nothing -> liftIO $ putStrLn "No expression to evaluate";
    Just (Decl _ it) -> output typeEnv' (runEval' termEnv' it)
  where
    isIt :: Decl -> Bool
    isIt (Decl "it" _) = True
    isIt _             = False

    output :: TypeEnv -> Either InterpreterError Value -> Repl ()
    output _ (Left err) = liftIO $ printDoc prettyInterpreterError err
    output tenv (Right val) =
      let scheme = fromJust $ TypeEnv.typeOf tenv (Var "it")
       in liftIO $ printDoc prettyIt (val, scheme)

    prettyIt :: (Value, Scheme) -> Doc AnsiStyle
    prettyIt (val, scheme) = vcat
      [ prettyValue val
      , prettySignature ("it", scheme)
      ]

-- Commands

browse :: [String] -> Repl ()
browse _ = do
  Ctx{..} <- get
  liftIO $ printDoc prettyTypeEnv typeEnv

load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ readFile (unwords args)
  exec True contents

typeOf :: [String] -> Repl ()
typeOf args = do
  Ctx{..} <- get
  let arg = unwords args
  case TypeEnv.typeOf typeEnv (Var $ Text.pack arg) of
    Nothing -> exec False arg
    Just scheme -> liftIO $ printDoc prettySignature (Text.pack arg, scheme)

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
    _ -> putStrLn "Invalid arguments"

run :: Repl a -> IO ()
run init = flip evalStateT Ctx.empty $
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

completer :: (Monad m, MonadState Ctx m) => WordCompleter m
completer n = do
  tenv <- gets typeEnv
  let defs = Text.unpack <$> TypeEnv.vars tenv
      cmds = [":load", ":browse", ":type", ":quit"]
      cmps = cmds ++ defs
  pure $ filter (List.isPrefixOf n) cmps

completerStyle :: CompleterStyle (StateT Ctx IO)
completerStyle = Prefix (wordCompleter completer) defaultMatcher

printDoc :: (a -> Doc AnsiStyle) -> a -> IO ()
printDoc f = putStrLn . Text.unpack . renderAnn f
