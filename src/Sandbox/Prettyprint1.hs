module Sandbox.Prettyprint1 where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)

-- (x <+> y) concatenates document x and y with a space in between.
--
-- x <+> y = x <> space <> y

-- λ> "xxx" <+> "yyy"
-- xxx yyy

-- λ> "lorem" <+> vsep ["ipsum", "dolor"]
-- lorem ipsum
-- dolor
-- λ>
-- λ> "lorem" <+> align (vsep ["ipsum", "dolor"])
-- lorem ipsum
--       dolor

prettyType :: [Doc ann] -> Doc ann
prettyType =
    align
  . sep
  . zipWith (<+>) ("::" : repeat "->")

prettyDecl :: Pretty a => a -> [Doc ann] -> Doc ann
prettyDecl n tys = pretty n <+> prettyType tys

doc :: Doc ann
doc = prettyDecl ("example" :: Text) ["Int", "Bool", "Char", "IO ()"]

ex1 :: IO ()
ex1 = putDocW 80 doc

ex2 :: IO ()
ex2 = putDocW 20 doc

-- λ> ex1
-- example :: Int -> Bool -> Char -> IO ()λ>
-- λ> ex2
-- example :: Int
--         -> Bool
--         -> Char
--         -> IO ()λ>
