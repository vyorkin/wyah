{
module Wyah.Chapter5.STLC.Parser (parse) where

import qualified Data.Text as Text
import Control.Monad.Except (Except, throwError)

import Wyah.Chapter5.STLC.Syntax (Expr(..), Lit(..), BinOp(..), Name(..), Type(..))
import Wyah.Chapter5.STLC.Lexer (Token(..))
import qualified Wyah.Chapter5.STLC.Lexer as Lexer
}

%name parse

%tokentype { Token }

%monad { Except String } { (>>=) } { return }

%error { parseError }

%token
  true  { TokTrue }
  false { TokFalse }
  '.'   { TokDot }
  '->'  { TokArr }
  '\\'  { TokLam }
  '+'   { TokAdd }
  '-'   { TokSub }
  '*'   { TokMul }
  '=='  { TokEq  }
  '('   { TokParenL }
  ')'   { TokParenR }
  NUM   { TokNum $$ }
  VAR   { TokVar $$ }
  ':'   { TokColon }
  Int   { TokInt }
  Bool  { TokBool }

%left '+' '-'
%left '*'
%%

expr :: { Expr }
expr : '\\' VAR ':' type '.' expr { ELam (Name (Text.pack $2), $4) $6 }
     | form                       { $1 }

form : form '+'  form { EOp Add $1 $3 }
     | form '-'  form { EOp Sub $1 $3 }
     | form '*'  form { EOp Mul $1 $3 }
     | form '==' form { EOp Eq  $1 $3 }
     | fact           { $1 }

fact : fact atom { EApp $1 $2 }
     | atom      { $1 }

atom : '(' expr ')' { $2 }
     | NUM          { ELit (LInt $1) }
     | true         { ELit (LBool True) }
     | false        { ELit (LBool False) }
     | VAR          { EVar (Name (Text.pack $1)) }

type : '(' type ')'   { $2 }
     | Int            { TInt }
     | Bool           { TBool }
     | type '->' type { TArr $1 $3 }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"
}
