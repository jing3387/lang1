module Language.Schminke.Frontend.Type where

newtype TVar =
  TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type
         Type
  deriving (Show, Eq, Ord)

data Scheme =
  Forall [TVar]
         Type
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"
