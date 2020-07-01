{
module Wyah.Chapter7.Lexer
  ( Lexeme(..)
  , Token(..)
  , Alex
  , showPosn
  , runAlex
  , alexGetInput
  , alexError
  , alexMonadScan
  , lexer
  ) where

import Prelude hiding (GT, LT, EQ, lex)

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
}

%wrapper "monadUserState"

-- -----------------------------------------------------------------------------
-- Macroses and character sets
-- -----------------------------------------------------------------------------

$ws    = [\ \t\b]
$digit = 0-9
$alpha = [A-Za-z]

@number = [$digit]+
@var    = $alpha($alpha|_|$digit)*

-- -----------------------------------------------------------------------------
-- Tokens
-- -----------------------------------------------------------------------------

tokens :-

<0> "--".*  ;
<0> "let"   { mkT LLet }
<0> "rec"   { mkT LRec }
<0> "in"    { mkT LIn  }
<0> "if"    { mkT LIf }
<0> "then"  { mkT LThen }
<0> "else"  { mkT LElse }
<0> "fix"   { mkT LFix }
<0> "true"  { mkT LTrue }
<0> "false" { mkT LFalse }
<0> \(      { mkT LParenL }
<0> \)      { mkT LParenR }
<0> "->"    { mkT LArrow }
<0> \\      { mkT LLam }
<0> \+      { mkT LPlus }
<0> \-      { mkT LMinus }
<0> \*      { mkT LTimes }
<0> "=="    { mkT LEq }
<0> \=      { mkT LAssign }
<0> \;      { mkT LSemi }
<0> \n      { skip }
<0> $ws+    ;
<0> @number { mkNum }
<0> @var    { mkVar }
<0> .       { \_ _ -> lexerError "Illegal character" }


{
-- -----------------------------------------------------------------------------
-- Lexeme
-- -----------------------------------------------------------------------------

data Lexeme
  = LLet
  | LRec
  | LIn
  | LAssign
  | LIf
  | LThen
  | LElse
  | LFix
  | LTrue
  | LFalse
  | LParenL
  | LParenR
  | LLam
  | LArrow
  | LPlus
  | LMinus
  | LTimes
  | LEq
  | LNum !Int
  | LVar !Text
  | LSemi
  | LEOF
  deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- Token
-- -----------------------------------------------------------------------------

data Token = T
  !AlexPosn     -- Position info
  !Lexeme       -- Token lexeme
  !(Maybe Text) -- Raw matching string

instance Show Token where
  show (T _ LEOF _)  = "Token EOF"
  show (T pos l str) = "Token "
    ++ show l
    ++ " " ++ showPosn pos ++ " "
    ++ maybe "" show str

posn :: Token -> AlexPosn
posn (T pos _ _) = pos

mkT :: Lexeme -> AlexInput -> Int -> Alex Token
mkT lex (pos, _, _, str) len = pure $ T pos lex raw where
  raw :: Maybe Text
  raw = Just $ Text.pack (take len str)

isEOF :: Token -> Bool
isEOF (T _ lex _) = lex == LEOF

-- -----------------------------------------------------------------------------
-- Position info
-- -----------------------------------------------------------------------------

type Pos = Maybe AlexPosn

-- | Extracts line and column from a given 'Pos'.
position :: Pos -> (Int, Int)
position Nothing = (0, 0)
position (Just (AlexPn _ line column)) = (line, column)

-- | Given a position info returns "line:col".
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line column) = show line ++ ':': show column

-- -----------------------------------------------------------------------------
-- States
-- -----------------------------------------------------------------------------

state_initial :: Int
state_initial = 0

-- -----------------------------------------------------------------------------
-- Actions
-- -----------------------------------------------------------------------------

-- | Type of the token action (according to Alex User Guide).
type Action = AlexInput -> Int -> Alex Token

mkAction :: (Text -> Lexeme) -> Action
mkAction mkL (pos, _, _, input) len = do
  let raw = Text.pack $ take len input
      lex = mkL raw
  pure $ T pos lex (Just raw)

mkVar, mkNum :: Action
mkVar = mkAction LVar
mkNum = mkAction (LNum .  read . Text.unpack)

-- -----------------------------------------------------------------------------
-- The user state monad
-- -----------------------------------------------------------------------------

data AlexUserState = AlexUserState
  { -- Used by the lexer phase
    lexerStringState :: Bool
  , lexerStringValue :: String
    -- Used by the parser phase
  , parserCurrentToken :: Token
  , parserPos :: Pos
  }

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState { alex_ust = ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { lexerStringState = ss } }, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState { alex_ust = ust } -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { lexerStringValue = ss } }, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { lexerStringValue = c:lexerStringValue (alex_ust s) } }, ())

getParserCurrentToken :: Alex Token
getParserCurrentToken = Alex $ \s@AlexState { alex_ust = ust } -> Right (s, parserCurrentToken ust)

setParserCurrentToken :: Token -> Alex ()
setParserCurrentToken ss = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { parserCurrentToken = ss } }, ())

getParserPos :: Alex Pos
getParserPos = Alex $ \s@AlexState { alex_ust = ust } -> Right (s, parserPos ust)

setParserPos :: Pos -> Alex ()
setParserPos ss = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { parserPos = ss } }, ())

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { lexerStringState   = False
  , lexerStringValue   = ""
  , parserCurrentToken = T dummyPosn LEOF Nothing
  , parserPos          = Nothing
  }

-- -----------------------------------------------------------------------------
-- Helper/utility definitions
-- -----------------------------------------------------------------------------

dummyPosn :: AlexPosn
dummyPosn = AlexPn 0 0 0

-- -----------------------------------------------------------------------------
-- Definition needed by Alex
-- -----------------------------------------------------------------------------

alexEOF :: Alex Token
alexEOF = pure $ T dummyPosn LEOF Nothing

-- -----------------------------------------------------------------------------
-- Execution
-- -----------------------------------------------------------------------------

assertEOFState :: Alex ()
assertEOFState = do
  ss <- getLexerStringState
  when ss $ alexError "String not closed at end of file"

scanToken :: Alex Token
scanToken = do
  (tok, err) <- complementError alexMonadScan
  when (isJust err) $ lexerError (fromJust err)
  when (isEOF tok) $ assertEOFState
  pure tok

-- | Reports error with the given message.
lexerError :: String -> Alex a
lexerError msg = do
  (p, c, _, inp) <- alexGetInput
  let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
  let inp2 = if length inp1 > 30
             then trim (take 30 inp1)
             else trim inp1
  let disp = if null inp
             then " at end of file"
             else if null inp2
                  then " before end of line"
                  else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
  let disp3 = if null msg
              then "Lexer error"
              else trim msg
  alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | We capture the error message in order to complement it with the file position
complementError :: Alex a -> Alex (a, Maybe String)
complementError (Alex al) =
  Alex (\s -> case al s of
    Right (s', x) -> Right (s', (x, Nothing))
    Left  message -> Right (s, (undefined, Just message)))

-- Code generated by Alex

-- To invoke a scanner under the monad wrapper, use:
-- alexMonadScan :: Alex a

-- | The lexer function to be passed to Happy.
lexer :: (Token -> Alex a) -> Alex a
lexer cont = alexMonadScan >>= cont
}
