module Language.Schminke.Backend.Core where

type Name = String

data Expr
  = Lit Literal
  -- Lambda bound variables.
  | Var Word
  -- Let bound variables introduced by A-normalization.
  | Alpha Word
  -- Definition references.
  | Delta Name
  | Lambda Expr
  -- Let is a meta-construct introduced by A-normalization. Let expressions from
  -- the frontend get desugared into lambdas.
  | Let Word
        Expr
        Expr
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
