module Language.Schminke.Frontend.Syntax where

import Language.Schminke.Frontend.Type

data Expr
  = Lit Literal
  | Var Name
  | Lam Name Expr
  | Let Name Expr Expr
  | If Expr Expr Expr
  | App Expr Expr
  deriving (Show, Eq, Ord)

data Literal =
  Int Integer
  deriving (Show, Eq, Ord)

data Top
  = Dec Name
        Scheme
  | Def Name
        Expr
  deriving (Show, Eq, Ord)

data Program =
  Program [Top]
          (Maybe Expr)
  deriving (Show, Eq, Ord)

definitions :: [Top] -> [Top]
definitions [] = []
definitions (def@Def {}:rest) = def : definitions rest
definitions (_:rest) = definitions rest

declarations :: [Top] -> [(Name, Scheme)]
declarations [] = []
declarations (dec@(Dec x sc):rest) = (x, sc) : declarations rest
declarations (_:rest) = declarations rest
