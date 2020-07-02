module Wyah.Chapter7.PrettyTest where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString

import Test.Tasty (TestTree, testGroup)

import Wyah.Chapter7.Pretty (ppProgram, ppDecl, ppExpr, ppType)
import Wyah.Support (glob, golden)

test_ppProgram :: IO TestTree
test_ppProgram = ppTest "ppProgram" "program" ".syn" ppProgram

test_ppDecl :: IO TestTree
test_ppDecl = ppTest "ppDecl" "decl" ".syn" ppDecl

test_ppExpr :: IO TestTree
test_ppExpr = ppTest "ppExpr" "expr" ".syn" ppExpr

test_ppType :: IO TestTree
test_ppType = ppTest "ppType" "type" ".typ" ppType

ppTest
  :: Read a
  => String      -- ^ Test group name
  -> String      -- ^ Relative directory
  -> String      -- ^ Second/additional extension (including dot)
  -> (a -> Text) -- ^ Printer function
  -> IO TestTree
ppTest group dir ext pp = do
  paths <- glob (basePath ++ dir) ("*.lc" ++ ext)
  tests <- mapM (golden $ ppFile pp) paths
  pure $ testGroup group tests
  where basePath = "test/Wyah/Chapter7/tests/pretty/"

ppFile :: Read a => (a -> Text) -> FilePath -> IO ByteString
ppFile pp path = do
  expr <- read <$> readFile path
  pure $ ByteString.fromStrict $ encodeUtf8 (pp expr)
