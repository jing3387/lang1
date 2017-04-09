module Language.Schminke.Frontend.Syntax where

import Language.Schminke.Frontend.Type

type Name = String

data Expr
  = Lit Literal
  | Var Name
  | Binop Binop
          Expr
          Expr
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

data Binop
  = Add
  | Sub
  | Mul
  | Sdiv
  | Srem
  | Eq
  deriving (Eq, Ord, Show)

type BinopDef = (String, Binop, Type)

binops :: [BinopDef]
binops =
  [ ("add", Add, typeInt `TArr` (typeInt `TArr` typeInt))
  , ("sub", Sub, typeInt `TArr` (typeInt `TArr` typeInt))
  , ("mul", Mul, typeInt `TArr` (typeInt `TArr` typeInt))
  , ("sdiv", Sdiv, typeInt `TArr` (typeInt `TArr` typeInt))
  , ("srem", Srem, typeInt `TArr` (typeInt `TArr` typeInt))
  , ("eq", Eq, typeInt `TArr` (typeInt `TArr` typeBool))
  ]

type Def = (String, Expr)

data Program =
  Program [Def]
          Expr
  deriving (Eq)
