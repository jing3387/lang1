module Language.Schminke.Backend.Core where

type Name = String

data Expr
  = Lit Literal
  | Var Word
  | Binop Binop
          Expr
          Expr
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

type Def = (String, Expr)
