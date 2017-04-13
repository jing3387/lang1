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
  | TRef Type
  deriving (Show, Eq, Ord)

data Scheme =
  Forall [TVar]
         Type
  deriving (Show, Eq, Ord)

i :: Integer -> Type
i n = TCon ("i" ++ show n)

sum :: Type -> Type -> Type
sum (TSum t1a t1b) (TSum t2a t2b) =
  foldr1 TSum (nub $ [t1a, t1b, t2a, t2b])
sum s@(TSum t1a t1b) t2 =
  if t2 `elem` [t1a, t1b] || t2 == s
    then s
    else TSum t1a (TSum t1b t2)
sum t1 s@(TSum t2a t2b) =
  if t1 `elem` [t2a, t2b] || t1 == s
    then s
    else TSum t1 (TSum t2a t2b)
sum t1 t2 =
  if t1 == t2
    then t1
    else TSum t1 t2

prims :: [(Name, Scheme)]
prims =
  [ ( "eq", Forall [TV "a"] (TArr (TVar (TV "a")) (TArr (TVar (TV "a")) (i 1))))
  , ("add", Forall [TV "a"] (TArr (TVar (TV "a")) (TArr (TVar (TV "a")) (TVar (TV "a")))))
  , ("sub", Forall [TV "a"] (TArr (TVar (TV "a")) (TArr (TVar (TV "a")) (TVar (TV "a")))))
  , ("mul", Forall [TV "a"] (TArr (TVar (TV "a")) (TArr (TVar (TV "a")) (TVar (TV "a")))))
  , ("sdiv", Forall [TV "a"] (TArr (TVar (TV "a")) (TArr (TVar (TV "a")) (TVar (TV "a")))))
  , ("srem", Forall [TV "a"] (TArr (TVar (TV "a")) (TArr (TVar (TV "a")) (TVar (TV "a")))))
  ]
