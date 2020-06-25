module Sandbox.Prettyprint1 where

import System.IO (stdout)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, color, Color(..), bold, underlined)

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

style :: AnsiStyle
style = color Green <> bold

styledDoc :: Doc AnsiStyle
styledDoc = annotate style "hello world"

render :: Doc AnsiStyle -> IO ()
render = renderIO stdout . layoutPretty defaultLayoutOptions

doc' :: Doc AnsiStyle
doc' = annotate (color Red) ("red" <+> align (vsep [annotate (color Blue <> underlined) ("blue+u" <+> annotate bold "bold" <+> "blue+u"), "red"]))

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
