module Language.Schminke.Backend.Core where

type Name = String

data Expr
  = Lit Literal
  | Var Word
  | Prim Name
  | Lam Expr
  | App Expr
        Expr
  deriving (Show, Eq, Ord)

data Literal =
  Int Integer
  deriving (Show, Eq, Ord)

data Binop
  = Add
  | Sub
  | Mul
  | Sdiv
  | Srem
  | Eq
  deriving (Eq, Ord, Show)

data Top =
  Def Name
      Expr
  deriving (Show, Eq, Ord)

prims :: [String]
prims = ["add", "sub", "mul", "sdiv", "srem", "eq"]
