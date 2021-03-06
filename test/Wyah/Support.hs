module Wyah.Support
  ( golden
  , glob

  , showEither
  , textToBs
  ) where

import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.FilePath (takeBaseName, replaceExtension)
import System.FilePath.Glob (globDir1, compile)

import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsStringDiff)

golden :: (FilePath -> IO ByteString) -> FilePath -> IO TestTree
golden run path = do
  let goldenPath = replaceExtension path ".golden"
  pure $ goldenVsStringDiff (takeBaseName path) diff goldenPath (run path)
  where diff ref new = ["diff", "-u", ref, new]

glob :: FilePath -> String -> IO [FilePath]
glob dir pat = globDir1 (compile pat) dir

showEither :: (Show a, Show b) => Either a b -> String
showEither = either show show

textToBs :: Text -> ByteString
textToBs = ByteString.fromStrict . encodeUtf8
