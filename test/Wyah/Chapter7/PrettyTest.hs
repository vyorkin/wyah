module Wyah.Chapter7.PrettyTest where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Text.Prettyprint.Doc (pretty)

import Test.Tasty (TestTree, testGroup)

import Wyah.Chapter7.Syntax (Program)
import Wyah.Chapter7.Type (Type)
import Wyah.Chapter7.Pretty (renderRaw, prettyProgram, prettyValue, prettySteps)
import Wyah.Support (glob, golden, textToBs)

test_ppProgram :: IO TestTree
test_ppProgram = ppTest "ppProgram" "program" ".syn" ppProgram

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
  pure $ textToBs (pp expr)

ppProgram :: Program -> Text
ppProgram = renderRaw prettyProgram

ppType :: Type -> Text
ppType = renderRaw pretty
