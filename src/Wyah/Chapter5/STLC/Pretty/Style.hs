module Wyah.Chapter5.STLC.Pretty.Style
  ( var
  , binOp
  , ty
  , lam
  , lit
  , dot
  , colon
  ) where

import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, color, colorDull, Color(..), bold)

var :: AnsiStyle
var = color Magenta

binOp :: AnsiStyle
binOp = colorDull White

ty :: AnsiStyle
ty = color Blue <> bold

lam :: AnsiStyle
lam = color Red <> bold

dot :: AnsiStyle
dot = colorDull White <> bold

lit :: AnsiStyle
lit = color Green

colon :: AnsiStyle
colon = color Cyan
