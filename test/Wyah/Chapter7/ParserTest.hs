module Wyah.Chapter7.ParserTest where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.FilePath (takeBaseName, replaceExtension)
import System.FilePath.Glob (globDir1, compile)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, goldenVsStringDiff)

import Wyah.Chapter7.Parser (parseProgram)

test_parser :: IO TestTree
test_parser = do
  paths <- listTestFiles
  goldens <- mapM mkGoldenTest paths
  pure $ testGroup "parser" goldens

listTestFiles :: IO [FilePath]
listTestFiles = globDir1 pat "test/Wyah/Chapter7/tests/parser"
  where pat = compile "*.lc"

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  let
    testName = takeBaseName path
    goldenPath = replaceExtension path ".golden"
  pure $ goldenVsStringDiff testName diff goldenPath action
  where
    diff :: FilePath -> FilePath -> [String]
    diff ref new = ["diff", "-u", ref, new]

    action :: IO ByteString
    action = do
      program <- readFile path
      let actual = either id show (parseProgram program)
      pure $ ByteString.pack actual
