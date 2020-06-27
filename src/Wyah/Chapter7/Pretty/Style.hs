module Wyah.Chapter7.Pretty.Style
  ( var
  , binOp
  , ty
  , lam
  , lit
  , arrow
  , colon
  , cond
  , letin
  , fix
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

arrow :: AnsiStyle
arrow = colorDull White <> bold

lit :: AnsiStyle
lit = color Green

colon :: AnsiStyle
colon = color Cyan

cond :: AnsiStyle
cond = color Red

letin :: AnsiStyle
letin = color Red

fix :: AnsiStyle
fix = color Yellow
