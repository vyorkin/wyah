module Wyah.Chapter7.ParserTest where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString

import Test.Tasty (TestTree, testGroup)

import Wyah.Chapter7.Parser (parseProgram')
import Wyah.Support (glob, golden)

test_parser :: IO TestTree
test_parser = do
  paths <- glob "test/Wyah/Chapter7/tests/parser" "*.lc"
  tests <- mapM (golden parseFile) paths
  pure $ testGroup "parser" tests

parseFile :: FilePath -> IO ByteString
parseFile path = do
  program <- readFile path
  let actual = either id show (parseProgram' program)
  pure $ ByteString.pack actual
