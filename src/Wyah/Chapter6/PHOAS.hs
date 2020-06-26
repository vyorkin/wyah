{-# LANGUAGE RankNTypes #-}

module Wyah.Chapter6.PHOAS
  ( ExprP(..)
  , Expr(..)
  , Value(..)
  , fromVFun
  , fromVLit
  , eval
  , i
  , k
  , s
  , skk
  , test
  ) where

data ExprP a
  = VarP a
  | AppP (ExprP a) (ExprP a)
  | LamP (a -> ExprP a)
  | LitP Integer

newtype Expr = Expr { unExpr :: forall a. ExprP a }

i :: ExprP a
i = LamP (\a -> VarP a)

k :: ExprP a
k = LamP (\x -> LamP (\_ -> VarP x))

s :: ExprP a
s =
  LamP (\f ->
    LamP (\g ->
      LamP (\x ->
        AppP
          (AppP (VarP f) (VarP x))
          (AppP (VarP g) (VarP x)))))

data Value
  = VLit Integer
  | VFun (Value -> Value)

fromVFun :: Value -> (Value -> Value)
fromVFun (VFun f) = f
fromVFun _        = error "not a function"

fromVLit :: Value -> Integer
fromVLit (VLit n) = n
fromVLit _        = error "not an integer"

eval :: Expr -> Value
eval e = ev (unExpr e) where
  ev (VarP v)     = v
  ev (AppP e1 e2) = fromVFun (ev e1) (ev e2)
  ev (LamP f)     = VFun (ev . f)
  ev (LitP n)     = VLit n

skk :: ExprP a
skk = AppP (AppP s k) k

test :: Integer
test = fromVLit $ eval $ Expr (AppP skk (LitP 3))
