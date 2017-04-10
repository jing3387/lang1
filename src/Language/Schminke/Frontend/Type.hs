module Language.Schminke.Frontend.Type where

import Data.List

type Name = String

newtype TVar =
  TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TPro Type
         Type
  | TSum Type
         Type
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

typeSum :: Type -> Type -> Type
typeSum sum1@(TSum t1a t1b) sum2@(TSum t2a t2b) =
  foldr1 TSum (nub $ [t1a, t1b, t2a, t2b])
typeSum sum@(TSum t1a t1b) t2 =
  if t2 `elem` [t1a, t1b] || t2 == sum
    then sum
    else TSum t1a (TSum t1b t2)
typeSum t1 sum@(TSum t2a t2b) =
  if t1 `elem` [t2a, t2b] || t1 == sum
    then sum
    else TSum t1 (TSum t2a t2b)
typeSum t1 t2 =
  if t1 == t2
    then t1
    else TSum t1 t2

typeOps :: [String]
typeOps = ["->", "+", "*"]

intBinop :: Scheme
intBinop = Forall [] (TArr typeInt (TArr typeInt typeInt))

prims :: [(Name, Scheme)]
prims =
  [ ( "eq"
    , Forall
        [(TV "a"), (TV "b")]
        (TArr
           typeInt
           (TArr
              typeInt
              (TArr
                 (TVar (TV "a"))
                 (TArr (TVar (TV "b")) (TSum (TVar (TV "a")) (TVar (TV "b"))))))))
  , ("add", intBinop)
  , ("sub", intBinop)
  , ("mul", intBinop)
  , ("sdiv", intBinop)
  , ("srem", intBinop)
  ]
