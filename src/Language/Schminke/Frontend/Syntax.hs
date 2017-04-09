module Language.Schminke.Frontend.Syntax where

import Language.Schminke.Frontend.Type

data Expr
  = Lit Literal
  | Var Name
  | Lam Name
        Expr
  | Let Name
        Expr
        Expr
  | If Expr
       Expr
       Expr
  | App Expr
        Expr
  deriving (Show, Eq, Ord)

data Literal =
  Int Integer
  deriving (Show, Eq, Ord)

data Top
  = Dec Name
        Type
  | Def Name
        Expr
  deriving (Show, Eq, Ord)

data Program =
  Program [Top]
          Expr
  deriving (Eq)
