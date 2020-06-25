module Wyah.Chapter5.Calc.REPL
  ( main
  , repl
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)
import System.IO (hSetBuffering, BufferMode(..), stdout)
import System.IO.Error (isEOFError, catchIOError)
import qualified Codec.Binary.UTF8.String as String

import Wyah.Chapter5.Calc.Syntax (Expr(..))
import Wyah.Chapter5.Calc.Type (Type(..))
import Wyah.Chapter5.Calc.Lexer (evalP)
import Wyah.Chapter5.Calc.Parser (parse)
import Wyah.Chapter5.Calc.Check (check)
import Wyah.Chapter5.Calc.Pretty (renderExpr, renderType)
import Wyah.Chapter5.Calc.Eval (eval)

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl

repl :: IO ()
repl = do
  putStr "?> "
  line <- liftIO $ catchIOError (Just <$> getLine) eofHandler
  maybe (return ()) handle line
  repl

handle :: String -> IO ()
handle s = case evalP parse (String.encode s) of
  Left e  -> putStrLn e
  Right r -> putStrLn $ (Text.unpack (output r))

eofHandler :: IOError -> IO (Maybe String)
eofHandler e
  | isEOFError e = return Nothing
  | otherwise = ioError e

output :: Expr -> Text
output e = case check e of
  Left s -> Text.pack $ show s
  Right t ->
    let r = eval e
     in case r of
       Nothing -> "⊥"
       Just re -> ppResult e re t

ppResult :: Expr -> Expr -> Type -> Text
ppResult e r t =
     Text.pack (show e)
  <> "\n"
  <> renderExpr r
  <> " : "
  <> renderType t
