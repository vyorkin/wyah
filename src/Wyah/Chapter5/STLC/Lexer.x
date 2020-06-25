{
module Wyah.Chapter5.STLC.Lexer
  ( Token(..)
  , scan
  ) where

import Control.Monad.Except (Except, throwError)
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Za-z]
$eol = [\n]
@var = $alpha($alpha|_|$digit)*

tokens :-
  $eol    ;
  $white+ ;
  "#".*   ;
  true    { \_ -> TokTrue }
  false   { \_ -> TokFalse }
  "->"    { \_ -> TokArr }
  "."     { \_ -> TokDot }
  \\      { \_ -> TokLam }
  $digit+ { \s -> TokNum (read s) }
  \+      { \_ -> TokAdd }
  \-      { \_ -> TokSub }
  \*      { \_ -> TokMul }
  "=="    { \_ -> TokEq }
  \(      { \_ -> TokParenL }
  \)      { \_ -> TokParenR }
  "Int"   { \_ -> TokInt }
  "Bool"  { \_ -> TokBool }
  @var    { \s -> TokVar s }
  \:      { \_ -> TokColon }

{
data Token
  = TokTrue
  | TokFalse
  | TokArr
  | TokDot
  | TokLam
  | TokNum Int
  | TokVar String
  | TokColon
  | TokInt
  | TokBool
  | TokAdd
  | TokSub
  | TokMul
  | TokEq
  | TokParenL
  | TokParenR
  | TokEOF
  deriving (Eq, Show)

scan :: String -> Except String [Token]
scan str = go ('\n', [], str) where
  go inp@(_, _, s) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme."
     AlexSkip  inp' _ -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len s)
      return (rest : res)
}
