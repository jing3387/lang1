module Language.Schminke.Backend.Reduce
  ( shift
  , subst
  , beta
  ) where

import Language.Schminke.Backend.Core

shift :: Int -> Expr -> Expr
shift d t = walk 0 t
  where
    walk :: Int -> Expr -> Expr
    walk c t =
      case t of
        x@Lit {} -> x
        x@(Var n) ->
          if n >= c && c >= 0
            then Var (n + d)
            else x
        x@Pop {} -> x
        x@Del {} -> x
        Lam t1 -> Lam $ walk (c + 1) t1
        Let x t1 t2 -> Let x (walk c t1) (walk c t2)
        If cond tr fl -> If (walk c cond) (walk c tr) (walk c fl)
        App t1 t2 -> App (walk c t1) (walk c t2)

subst :: Int -> Expr -> Expr -> Expr
subst j s t = walk 0 t
  where
    walk :: Int -> Expr -> Expr
    walk c t =
      case t of
        x@Lit {} -> x
        x@(Var n) ->
          if n == j + c && c >= 0
            then shift c s
            else x
        x@Pop {} -> x
        x@Del {} -> x
        Lam t1 -> Lam $ walk (c + 1) t1
        Let x t1 t2 -> Let x (walk c t1) (walk c t2)
        If cond tr fl -> If (walk c cond) (walk c tr) (walk c fl)
        App t1 t2 -> App (walk c t1) (walk c t2)

beta :: Expr -> Expr -> Expr
beta s t = shift (-1) (subst 0 (shift 1 s) t)
