module Wyah.Chapter7.EvalTest where

import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString

import Test.Tasty (TestTree, testGroup)

import Wyah.Chapter7.Syntax (Expr)
import Wyah.Chapter7.Parser (parseExpr')
import Wyah.Chapter7.Eval (Value, InterpreterError, runEval', emptyTermEnv)
import Wyah.Chapter7.Pretty (renderRaw, prettyValue, prettyInterpreterError)

import Wyah.Support (glob, golden, textToBs)

test_eval :: IO TestTree
test_eval = do
  paths <- glob "test/Wyah/Chapter7/tests/eval/expr" "*.lc"
  tests <- mapM (golden evalFile) paths
  pure $ testGroup "eval" tests

evalFile :: FilePath -> IO ByteString
evalFile path = do
  contents <- readFile path
  let res = parseExpr' contents
  pure $ tryEvalExpr res

tryEvalExpr :: Either String Expr -> ByteString
tryEvalExpr (Left err) = ByteString.pack err
tryEvalExpr (Right expr) =
  let actual = ppResult $ evalExpr expr
   in textToBs actual

evalExpr :: Expr -> Either InterpreterError Value
evalExpr = runEval' emptyTermEnv

ppResult :: Either InterpreterError Value -> Text
ppResult = either ppError ppValue

ppError :: InterpreterError -> Text
ppError = renderRaw prettyInterpreterError

ppValue :: Value -> Text
ppValue = renderRaw prettyValue
