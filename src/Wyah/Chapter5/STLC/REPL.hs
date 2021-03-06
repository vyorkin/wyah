module Wyah.Chapter5.STLC.REPL
  ( main
  , repl
  , output
  , handle
  , parseExpr
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExcept)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)
import System.IO.Error (isEOFError, catchIOError)

import Wyah.Chapter5.STLC.Syntax (Expr(..), Type(..))
import qualified Wyah.Chapter5.STLC.Lexer as Lexer
import Wyah.Chapter5.STLC.Parser (parse)
import Wyah.Chapter5.STLC.Check (checkTop, errorText)
import Wyah.Chapter5.STLC.Pretty (prettyExpr, prettyType)
import Wyah.Chapter5.STLC.Eval (Step(..), runEval)
import Wyah.Chapter5.STLC.Types (Value)
import qualified Wyah.Chapter5.STLC.Env as Env

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  repl

repl :: IO ()
repl = do
  putStr "?> "
  line <- liftIO $ catchIOError (Just <$> getLine) eofHandler
  maybe (pure ()) handle line
  repl

handle :: String -> IO ()
handle s = case parseExpr s of
  Left e  -> putStrLn e
  Right r -> putStrLn $ Text.unpack (output r)

eofHandler :: IOError -> IO (Maybe String)
eofHandler e
  | isEOFError e = pure Nothing
  | otherwise = ioError e

output :: Expr -> Text
output e = case checkTop Env.empty e of
  Left err ->
       Text.pack (show e)
    <> "\nError: "
    <> errorText err
  Right t ->
    let (r, es) = runEval e
        reds = Text.intercalate "\n" (stepText <$> es)
     in case r of
       Left msg -> Text.pack msg
       Right re -> resultText e re t reds

stepText :: Step -> Text
stepText (e :-> v) = e <> " --> " <> Text.pack (show v)

resultText :: Expr -> Value -> Type -> Text -> Text
resultText e r t reds =
     Text.pack (show e)
  <> "\n"
  <> prettyExpr e
  <> "\n"
  <> Text.pack (show r)
  <> " : "
  <> prettyType t
  <> "\nReductions:\n"
  <> reds

parseExpr :: String -> Either String Expr
parseExpr input = runExcept $ Lexer.scan input >>= parse
