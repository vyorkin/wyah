module Wyah.Chapter8.Frontend
  ( Expr(..)
  , Constr
  , Stmt(..)
  , Pattern(..)
  , Match(..)
  , Lit(..)
  , Module(..)
  , Decl(..)
  , ConDecl(..)
  , ScDecl(..)
  , Fixity(..)
  , Assoc(..)
  ) where

import GHC.Word (Word8)

import Wyah.Chapter8.Name (Name(..))
import Wyah.Chapter8.Type (Type(..), Pred(..))

-- | Type constructor.
type Constr = Name

data Expr
  = EVar Name           -- ^ x
  | EApp Expr Expr      -- ^ e1 e2
  | ELam Name Expr      -- ^ \\x. e
  | ELet Name Expr Expr -- ^ let x = y in e
  | ELit Lit            -- ^ 5, 'a', "string"
  | EIf Expr Expr Expr  -- ^ if c then t else f
  | ECase Expr [Match]  -- ^ case x of { p -> e; ... }
  | EAnn Expr Type      -- ^ (x : Int)
  | EDo [Stmt]          -- ^ do { ... }
  deriving (Eq, Read, Show)

-- | Do-block statement.
data Stmt
  = Generator Pattern Expr -- ^ pat <- expr
  | Qualified Expr         -- ^ expr
  deriving (Eq, Read, Show)

data Pattern
  = PVar Name             -- ^ x
  | PCon Constr [Pattern] -- ^ C x y
  | PLit Lit              -- ^ 3, 'a', "blah"
  | PWild                 -- ^ _
  deriving (Eq, Read, Show)

-- | Pattern-matching expression.
-- For example:
-- f (C1 x)   = ...
-- f (C2 x y) = ...
-- f _        = ...
data Match = Match [Pattern] Expr
  deriving (Eq, Read, Show)

-- | Literal.
data Lit
  = LInt Int        -- ^ 1
  | LChar Char      -- ^ 'a'
  | LString [Word8] -- ^ "str"
  deriving (Eq, Read, Show)

-- module Foo where
-- decl1 = ...
-- decl2 = ...
-- ..... = ...
-- decln = ...
data Module = Module Name [Decl]
  deriving (Eq, Read, Show)

-- | Declaration.
data Decl
  = DFn Type ScDecl                  -- ^ f x = x + 1
  | DType Type                       -- ^ t :: forall a b. a -> b -> Int
  | DData Constr [Name] [ConDecl]    -- ^ data T a b c = A a b | B c | ...
  | DClass [Pred] Name [Name] [Decl] -- ^ class (A a, B b, ...) => Foo a b where foo x = ...
  | DInst [Pred] Name Type [Decl]    -- ^ instance (A a, B a, ...) => T (Foo a) where f x = ...
  | DFixity Name Fixity              -- ^ infixl 1, infixr 2, ...
  deriving (Eq, Read, Show)

-- | Supercombinator declaration.
data ScDecl = ScDecl
  { scName  :: Name
  , scPats  :: [Match]
  , scType  :: Maybe Type
  , scWhere :: [Decl]
  } deriving (Eq, Read, Show)

-- | Type constructor declaration.
data ConDecl
  = ConSimpl Constr Type
  | ConRec Constr [(Name, Type)] Type
  deriving (Eq, Read, Show)

data Fixity
  = FInfix Assoc Int
  | FPrefix Int
  | FPostfix Int
  deriving (Eq, Read, Show)

data Assoc = L | R | N
  deriving (Eq, Read, Show)
