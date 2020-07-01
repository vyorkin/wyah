module Wyah.Chapter7.InferTest where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString

import Test.Tasty (TestTree, testGroup)

import Wyah.Chapter7.Infer (inferTop', inferTop, inferExpr', inferExpr)
import Wyah.Chapter7.TypeEnv (TypeEnv(..))
import qualified Wyah.Chapter7.TypeEnv as TypeEnv

import Wyah.Support (glob, golden, showEither)

-- test_inferExpr :: IO TestTree
-- test_inferExpr = do
--   paths <- glob "test/Wyah/Chapter7/tests/infer/expr" "*.lc.syn"
--   tests <- mapM (golden inferExprFile) paths
--   pure $ testGroup "inferExpr" tests

inferExprFile :: FilePath -> IO ByteString
inferExprFile path = do
  expr <- read <$> readFile path
  let actual = showEither $ inferExpr' expr
  pure $ ByteString.pack actual
