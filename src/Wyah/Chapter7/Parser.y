{
module Wyah.Chapter7.Parser (parse) where

import Prelude hiding (GT, LT, EQ)

import qualified Data.Text as Text
import Wyah.Chapter7.Syntax (Program(..), Decl(..), Expr(..), Lit(..), BinOp(..), Var(..))
import Wyah.Chapter7.Lexer (Alex, Lexeme(..), Token(..), lexer, showPosn)
}

%name parse

%tokentype { Token }

%lexer { lexer } { T _ LEOF _ }

%monad { Alex }

%error { parseError }

-- -----------------------------------------------------------------------------
-- Tokens
-- -----------------------------------------------------------------------------

%token
  'let'   { T _ LLet _ }
  'rec'   { T _ LRec _ }
  'in'    { T _ LIn _ }
  'if'    { T _ LIf _ }
  'then'  { T _ LThen _ }
  'else'  { T _ LElse _ }
  'fix'   { T _ LFix _  }
  'true'  { T _ LTrue _ }
  'false' { T _ LFalse _ }
  '('     { T _ LParenL _ }
  ')'     { T _ LParenR _ }
  '->'    { T _ LArrow _ }
  '\\'    { T _ LLam _ }
  '+'     { T _ LPlus _ }
  '-'     { T _ LMinus _ }
  '*'     { T _ LTimes _ }
  '=='    { T _ LEq _ }
  '='     { T _ LAssign _ }
  ';'     { T _ LSemi _ }
  VAR     { T _ (LVar $$) _ }
  NUM     { T _ (LNum $$) _ }

%left '=='
%left '+' '-'
%left '*' '/'

-- -----------------------------------------------------------------------------
-- Grammar rules
-- -----------------------------------------------------------------------------

%%

program :: { Program }
program : decls { Program $1 }

decls :: { [Decl] }
decls : decl { [$1] }
      | decls decl { $2 : $1 }

decl :: { Decl }
decl : decl1 ';' { $1 }

decl1 :: { Decl }
decl1 : 'let' VAR '=' expr { Decl $2 $4 }

expr :: { Expr }
expr : 'let' VAR '=' expr 'in' expr      { EApp (ELam (Var $2) $6) $4 }
     | '\\' VAR '->' expr                { ELam (Var $2) $4 }
     | 'fix' expr                        { EFix $2 }
     | 'if' expr 'then' expr 'else' expr { EIf $2 $4 $6 }
     | form                              { $1 }

form :: { Expr }
form : form '+'  form { EOp Add $1 $3 }
     | form '-'  form { EOp Sub $1 $3 }
     | form '*'  form { EOp Mul $1 $3 }
     | form '==' form { EOp Eq  $1 $3 }
     | fact           { $1 }

fact :: { Expr }
fact : fact atom { EApp $1 $2 }
     | atom      { $1 }

atom :: { Expr }
atom : '(' expr ')' { $2 }
     | 'true'       { ELit (LBool True) }
     | 'false'      { ELit (LBool False) }
     | NUM          { ELit (LInt (toInteger $1)) }
     | VAR          { EVar (Var $1) }

{
-- -----------------------------------------------------------------------------
-- Module trailer
-- -----------------------------------------------------------------------------

parseError :: Token -> Alex a
parseError (T pos l raw) = error $
     "Parsing error on lexeme "
  ++ show l
  ++ " at "
  ++ showPosn pos
  ++ maybe "" (\str -> ". Input: " ++ Text.unpack str) raw
}
