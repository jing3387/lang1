module Language.Schminke.Backend.Sub
  ( shift
  , subst
  , reduceExpr
  ) where

import Language.Schminke.Backend.Core

-- TODO: the only difference between shift and subst is what's done when a
-- variable is reached, generalize the solution. Possibly a good little task for
-- Jake.
shift :: Word -> Expr -> Expr
shift d t = walk 0 t
  where
    walk :: Word -> Expr -> Expr
    walk c t =
      case t of
        x@Lit {} -> x
        x@(Var n) ->
          if n >= c
            then Var (n + d)
            else x
        x@Alpha {} -> x
        x@Delta {} -> x
        Lambda t -> Lambda $ walk (c + 1) t
        App t1 t2 -> App (walk c t1) (walk c t2)

subst :: Word -> Expr -> Expr -> Expr
subst j s t = walk 0 t
  where
    walk :: Word -> Expr -> Expr
    walk c t =
      case t of
        x@Lit {} -> x
        x@(Var n) ->
          if n == j + c
            then shift c s
            else x
        x@Alpha {} -> x
        x@Delta {} -> x
        Lambda t -> Lambda $ walk (c + 1) t
        App t1 t2 -> App (walk c t1) (walk c t2)

reduce :: Expr -> Expr -> Expr
reduce s t = shift (-1) (subst 0 (shift 1 s) t)

reduceExpr :: Expr -> Expr
reduceExpr (App e1 e2) = reduceExpr $ reduce e2 e1
reduceExpr x = x
