{
module Wyah.Chapter7.Parser
  ( parseProgram
  , parseDecl
  , parseExpr

  , parseProgram'
  , parseDecl'
  , parseExpr'
  ) where

import Prelude hiding (GT, LT, EQ)

import qualified Data.Text as Text
import Wyah.Chapter7.Syntax (Program(..), Decl(..), Expr(..), Lit(..), BinOp(..), Var(..))
import Wyah.Chapter7.Lexer (Alex, Lexeme(..), Token(..), lexer, showPosn, runAlex)
}

%name parseProgram program
%name parseDecl decl
%name parseExpr expr

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
program : decls { Program (reverse $1) }

decls :: { [Decl] }
decls : decl { [$1] }
      | decls decl { $2 : $1 }

decl :: { Decl }
decl : decl1 ';' { $1 }

decl1 :: { Decl }
decl1 : declfn    { $1 }
      | declfnrec { $1 }
      | declval   { $1 }

declfn :: { Decl }
declfn : 'let' VAR vars '=' expr { Decl $2 (foldr ELam $5 (reverse $3)) }

declfnrec :: { Decl }
declfnrec : 'let' 'rec' VAR vars '=' expr { Decl $3 (EFix $ foldr ELam $6 ((Var $3) : (reverse $4))) }

declval :: { Decl }
declval : 'let' VAR '=' expr { Decl $2 $4 }

expr :: { Expr }
expr : letin { $1 }
     | lam   { $1 }
     | fix   { $1 }
     | cond  { $1 }
     | form  { $1 }

fix :: { Expr }
fix : 'fix' expr { EFix $2 }

cond :: { Expr }
cond : 'if'   expr
       'then' expr
       'else' expr { EIf $2 $4 $6 }

lam :: { Expr }
lam : '\\' var vars '->' expr { foldr ELam $5 ($2 : (reverse $3)) }

letin :: { Expr }
letin : 'let' rec var '=' expr
         'in' expr { ELet $3 $5 $7 }

rec :: { Bool }
rec : 'rec' { True }
    |       { False }

vars :: { [Var] }
vars :          { [] }
     | vars var { $2 : $1 }

var :: { Var }
var : VAR { Var $1 }

form :: { Expr }
form : binop { $1 }
     | fact  { $1 }

binop :: { Expr }
binop : form '+'  form { EOp Add $1 $3 }
      | form '-'  form { EOp Sub $1 $3 }
      | form '*'  form { EOp Mul $1 $3 }
      | form '==' form { EOp Eq  $1 $3 }

fact :: { Expr }
fact : fact atom { EApp $1 $2 }
     | atom      { $1 }

atom :: { Expr }
atom : '(' expr ')' { $2 }
     | true  { ELit $1 }
     | false { ELit $1 }
     | num   { ELit $1 }
     | var   { EVar $1 }

true :: { Lit }
true : 'true' { LBool True }

false :: { Lit }
false : 'false' { LBool False }

num :: { Lit }
num : NUM { LInt (toInteger $1) }

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

parseExpr' :: String -> Either String Expr
parseExpr' = flip runAlex parseExpr

parseDecl' :: String -> Either String Decl
parseDecl' = flip runAlex parseDecl

parseProgram' :: String -> Either String Program
parseProgram' = flip runAlex parseProgram
}
