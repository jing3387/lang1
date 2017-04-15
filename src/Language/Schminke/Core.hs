module Language.Schminke.Core where

import Language.Schminke.Type

data Expr
  = Lit Literal
  | Var (Name, Type)
  | Let [((Name, Type), Expr)]
        [Expr]
  | If Expr
       Expr
       Expr
  | App (Name, Type)
        [Expr]
  deriving (Show, Eq, Ord)

newtype Literal =
  Int Integer
  deriving (Show, Eq, Ord)

data Top =
  Def (Name, Type)
      [(Name, Type)]
      [Expr]
  deriving (Show, Eq, Ord)

type Program = [Top]
