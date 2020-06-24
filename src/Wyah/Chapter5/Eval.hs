module Wyah.Chapter5.Eval
  ( eval
  ) where

import Data.Maybe (fromMaybe)

import Wyah.Chapter5.Syntax (Expr(..))

-- Progress -- If an expression is well typed then either it is
-- a value, or it can be further evaluated by an available
-- evaluation rule.

-- Preservation -- If an expression e has type τ, and is
-- evaluated to e′, then e′ has type τ.

isNum :: Expr -> Bool
isNum = \case
  Zero   -> True
  Succ t -> isNum t
  _      -> False

isVal :: Expr -> Bool
isVal Tr          = True
isVal Fl          = True
isVal t | isNum t = True
isVal _           = False

nf :: Expr -> Expr
nf t = fromMaybe t (nf <$> eval1 t)

eval :: Expr -> Maybe Expr
eval e = case isVal e' of
  True  -> Just e'
  False -> Nothing
  where
    e' = nf e

eval1 :: Expr -> Maybe Expr
eval1 = \case
  Succ e                    -> Succ <$> (eval1 e)
  Pred Zero                 -> Just Zero
  Pred (Succ e) | isNum e   -> Just e
  Pred e                    -> Pred <$> (eval1 e)
  IsZero Zero               -> Just Tr
  IsZero (Succ e) | isNum e -> Just Fl
  IsZero e                  -> IsZero <$> (eval1 e)
  If Tr te _                -> Just te
  If Fl _ fe                -> Just fe
  If ce te fe               -> (\ce' -> If ce' te fe) <$> eval1 ce
  _                         -> Nothing
