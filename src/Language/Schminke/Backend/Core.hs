module Language.Schminke.Backend.Core where

type Name = String

data Expr
  = Lit Literal
  | Var Word
        Name
  | Lam Expr
  | Let Name
        Expr
        Expr
  | App Expr
        Expr
  deriving (Show, Eq, Ord)

data Literal =
  Int Integer
  deriving (Show, Eq, Ord)

type Def = (String, Expr)
