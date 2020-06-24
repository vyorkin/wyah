{
module Wyah.Chapter5.Lexer
  ( P
  , Token(..)
  , readToken
  , evalP
  , lexer
  ) where

import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Except (throwError)
import Data.Word (Word8)
}

tokens :-
  $white+ ;
  true    {TTrue}
  false   {TFalse}
  0       {TZero}
  succ    {TSucc}
  pred    {TPred}
  if      {TIf}
  then    {TThen}
  else    {TElse}
  iszero  {TIsZero}

{
data Token
  = TTrue
  | TFalse
  | TZero
  | TSucc
  | TPred
  | TIf
  | TThen
  | TElse
  | TIsZero
  | TEOF
  deriving (Show)

type AlexInput = [Word8]

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (b:bs) = Just (b, bs)
alexGetByte []     = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

type P a = StateT AlexInput (Either String) a

evalP :: P a -> AlexInput -> Either String a
evalP = evalStateT

-- | Function in our `P` monad which produces a new token
readToken :: P Token
readToken = do
  s <- get
  case alexScan s 0 of
    AlexEOF -> return TEOF
    AlexError _ -> throwError "!Lexical error"
    AlexSkip input _ -> do
      put input
      readToken
    AlexToken input _ token -> do
      put input
      return token

lexer :: (Token -> P a) -> P a
lexer cont = readToken >>= cont
}
