module Language.Schminke.Syntax where

import Language.Schminke.Type

data Expr
  = Lit Literal
  | Var Name
  | Let [(Name, Expr)] [Expr]
  | If Expr Expr Expr
  | App Name [Expr]
  deriving (Show, Eq, Ord)

data Literal = Int Integer
  deriving (Show, Eq, Ord)

data Top
  = Dec Name Scheme
  | Def Name [Name] [Expr]
  deriving (Show, Eq, Ord)

type Program = [Top]

definitions :: [Top] -> [Top]
definitions [] = []
definitions (def@Def {}:rest) = def : definitions rest
definitions (_:rest) = definitions rest

declarations :: [Top] -> [(Name, Scheme)]
declarations [] = []
declarations (dec@(Dec x sc):rest) = (x, sc) : declarations rest
declarations (_:rest) = declarations rest
