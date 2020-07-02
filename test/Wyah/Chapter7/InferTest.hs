module Wyah.Chapter7.InferTest where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString

import Test.Tasty (TestTree, testGroup)

import Wyah.Chapter7.Infer (inferTop', inferTop, inferDecl', inferDecl, inferExpr', inferExpr)
import Wyah.Chapter7.TypeEnv (TypeEnv(..))
import qualified Wyah.Chapter7.TypeEnv as TypeEnv

import Wyah.Support (glob, golden, showEither)

test_inferDecl :: IO TestTree
test_inferDecl = do
  paths <- glob "test/Wyah/Chapter7/tests/infer/decl" "*.lc.syn"
  tests <- mapM (golden inferDeclFile) paths
  pure $ testGroup "inferDecl" tests

inferDeclFile :: FilePath -> IO ByteString
inferDeclFile path = do
  expr <- read <$> readFile path
  let actual = showEither $ inferDecl' expr
  pure $ ByteString.pack actual
