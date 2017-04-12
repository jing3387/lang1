module Language.Schminke.Backend.Reduce
  ( shift
  , subst
  , beta
  , eta
  ) where

import Language.Schminke.Backend.Core

-- TODO: the only difference between shift and subst is what's done when a
-- variable is reached, generalize the solution. Possibly a good little task for
-- Jake.
shift :: Int -> Expr -> Expr
shift d t = walk 0 t
  where
    walk :: Int -> Expr -> Expr
    walk c t =
      case t of
        x@Lit {} -> x
        x@(Var n) ->
          if n >= c
            then Var (n + d)
            else x
        x@Del {} -> x
        Lam t1 -> Lam $ walk (c + 1) t1
        App t1 t2 -> App (walk c t1) (walk c t2)

subst :: Int -> Expr -> Expr -> Expr
subst j s t = walk 0 t
  where
    walk :: Int -> Expr -> Expr
    walk c t =
      case t of
        x@Lit {} -> x
        x@(Var n) ->
          if n == j + c
            then shift c s
            else x
        x@Del {} -> x
        Lam t1 -> Lam $ walk (c + 1) t1
        App t1 t2 -> App (walk c t1) (walk c t2)

beta :: Expr -> Expr -> Expr
beta s t = shift (-1) (subst 0 (shift 1 s) t)

eta :: Expr -> Expr
eta t =
  case t of
    x@Lit {} -> x
    x@Var {} -> x
    x@Del {} -> x
    Lam (App e1 (Var 0)) -> e1
    Lam e1 -> Lam $ eta e1
    App e1 e2 -> App (eta e1) (eta e2)
