module Wyah.Chapter7.Pretty.Utils
  ( parensIf
  ) where

import Data.Text.Prettyprint.Doc (Doc, parens)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id
