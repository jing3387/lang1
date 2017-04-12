module Language.Schminke.Backend.Core where

type Name = String

data Expr
  = Lit Literal
  | Var Int
  | Del Name
  | Lam Expr
  | App Expr
        Expr
  deriving (Show, Eq, Ord)

data Literal =
  Int Integer
  deriving (Show, Eq, Ord)

data Top =
  Def Name
      Expr
  deriving (Show, Eq, Ord)

data Program =
  Program [Top]
          (Maybe Expr)
  deriving (Show, Eq, Ord)

prims :: [String]
prims = ["add", "sub", "mul", "sdiv", "srem", "eq"]
