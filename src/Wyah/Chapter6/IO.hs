{-# LANGUAGE RankNTypes #-}

module Wyah.Chapter6.IO
  ( ExprP(..)
  , Expr(..)
  , Value(..)
  , fromVFun
  , fromVEff
  , fromVChar

  , run
  , eval

  , prim
  , lam
  , unary
  , binary

  , gets
  , puts
  , bind
  , seqn

  , ex1
  , ex2
  ) where

import Prelude hiding (seq)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char

type Name = Text

data ExprP a
  = VarP a
  | GlobalP Name
  | AppP (ExprP a) (ExprP a)
  | LamP (a -> ExprP a)
  | LitP Char
  | EffP a

newtype Expr = Expr { unExpr :: forall a. ExprP a }

data Value
  = VChar Char
  | VFun (Value -> Value)
  | VEff (IO Value)
  | VUnit

instance Show Value where
  show (VChar x) = show x
  show (VFun _)  = "<<function>>"
  show (VEff {}) = "<<effect>>"
  show (VUnit)   = "()"

fromVFun :: Value -> (Value -> Value)
fromVFun (VFun f) = f
fromVFun _        = error "not a function"

fromVEff :: Value -> (IO Value)
fromVEff (VEff f) = f
fromVEff _        = error "not an effect"

fromVChar :: Value -> Char
fromVChar (VChar v) = v
fromVChar _         = error "not a char"

run :: Expr -> IO ()
run f = void (fromVEff (eval f))

eval :: Expr -> Value
eval e = ev (unExpr e) where
  ev (LamP f)     = VFun (ev . f)
  ev (AppP e1 e2) = fromVFun (ev e1) (ev e2)
  ev (LitP n)     = VChar n
  ev (EffP v)     = v
  ev (VarP v)     = v
  ev (GlobalP op) = prim op

lam :: (Value -> Value) -> Value
lam = VFun

unary :: (Value -> Value) -> Value
unary f = lam $ \a -> f a

binary :: (Value -> Value -> Value) -> Value
binary f =
  lam $ \a ->
  lam $ \b -> f a b

prim :: Name -> Value
prim "putChar#" = unary \x -> VEff $ putChar (fromVChar x) >> pure VUnit
prim "getChar#" = VEff $ getChar >>= pure . VChar
prim "bindIO"   = binary bindIO
prim "returnIO" = unary returnIO
prim "thenIO"   = binary thenIO
prim name       = error $ "not implemented operation: " ++ Text.unpack name

gets, puts, bind, seqn :: ExprP a
gets = GlobalP "getChar#"
puts = GlobalP "putChar#"
bind = GlobalP "bindIO"
seqn = GlobalP "thenIO"

bindIO :: Value -> Value -> Value
bindIO (VEff f) (VFun g) = VEff (f >>= fromVEff . g)
bindIO _ _ = error "invalid argument types for bindIO"

returnIO :: Value -> Value
returnIO = VEff . pure

thenIO :: Value -> Value -> Value
thenIO (VEff f) (VEff g) = VEff (f >> g)
thenIO _ _ = error "thenIO applied to a non-eff"

ex1 :: IO ()
ex1 = run $ Expr (AppP (AppP bind gets) puts)

ex2 :: IO ()
ex2 = run $ Expr expr
  where
    expr :: ExprP a
    expr = foldr1 seq (str "Embedding IO with PHOAS!\n")

    seq :: ExprP a -> ExprP a -> ExprP a
    seq a b = AppP (AppP seqn a) b

    str :: forall a. String -> [ExprP a]
    str = fmap (\c -> AppP puts (LitP $ shf c))

    shf :: Char -> Char
    shf '\n' = '\n'
    shf c = Char.chr (Char.ord c + 1)
