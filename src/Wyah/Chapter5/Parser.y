{
module Wyah.Chapter5.Parser (parse) where

import Control.Monad.Error (throwError)

import Wyah.Chapter5.Syntax (Expr(..))
import Wyah.Chapter5.Lexer (P, Token (..), lexer)
}

%monad { P }
%lexer { lexer } { TEOF }
%name parse
%tokentype { Token }
%error { parseError }

%token
true   { TTrue }
false  { TFalse }
zero   { TZero }
succ   { TSucc }
pred   { TPred }
if     { TIf }
then   { TThen }
else   { TElse }
iszero { TIsZero }

-- Grammar rules

%%

Term
: true { Tr }
| false { Fl }
| zero { Zero }
| succ Term  { Succ $2 }
| pred Term { Pred $2 }
| if Term then Term else Term { If $2 $4 $6 }

{
parseError :: Token -> P a
parseError _ = throwError "!Parse error"
}
