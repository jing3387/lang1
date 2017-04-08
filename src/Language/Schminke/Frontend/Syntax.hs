module Language.Schminke.Frontend.Syntax where

type Name = String

data Expr
  = Lit Literal
  | Var Name
  | Lam Name
        Expr
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

data Program =
  Program [Def]
          Expr
  deriving (Eq)
