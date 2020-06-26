module Wyah.Chapter7.REPL
  ( Repl
  , Env(..)
  , setup
  , hoistErr

    -- * Commands
  , quit

  , run
  , main
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExcept)
import Control.Monad.State (StateT(..), evalStateT)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)
import System.IO.Error (isEOFError, catchIOError)
import System.Console.Repline (HaskelineT(..), abort, evalRepl, fileCompleter)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Wyah.Chapter7.Eval (TermEnv)
import Wyah.Chapter7.Infer (TypeEnv)
import Wyah.Chapter7.Env (Env)
import qualified Wyah.Chapter7.Env as Env

type Repl a = HaskelineT (StateT Env IO) a

setup :: IO ()
setup = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right v)  = pure v
hoistErr (Left err) = liftIO (print err) >> abort

-- Commands

quit :: () -> Repl ()
quit () = liftIO exitSuccess

run :: Repl a -> IO ()
run = undefined
-- run = flip evalStateT Env.empty . evalRepl "kek> " undefined

main :: IO ()
main = do
  args <- getArgs
  pure ()
