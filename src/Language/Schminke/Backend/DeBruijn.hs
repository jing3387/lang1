module Language.Schminke.Backend.DeBruijn
  ( debruijn
  ) where

import Data.List

import Language.Schminke.Backend.Core as Core
import Language.Schminke.Frontend.Syntax as Syntax

type Ctx = [(Syntax.Name, Word)]

debruijn :: Syntax.Expr -> Core.Expr
debruijn = debruijn' []
  where
    debruijn' :: Ctx -> Syntax.Expr -> Core.Expr
    debruijn' ctx expr =
      case expr of
        (Syntax.Lit (Syntax.Int n)) -> Core.Lit (Core.Int n)
        (Syntax.Var x) -> Core.Var n
          where n =
                  case lookup x ctx of
                    Nothing -> 0
                    Just n' -> n'
        (Syntax.Binop op e1 e2) ->
          Core.Binop (coreop op) (debruijn' ctx e1) (debruijn' ctx e2)
          where coreop :: Syntax.Binop -> Core.Binop
                coreop op =
                  case op of
                    Syntax.Add -> Core.Add
                    Syntax.Sub -> Core.Sub
                    Syntax.Mul -> Core.Mul
                    Syntax.Sdiv -> Core.Sdiv
                    Syntax.Srem -> Core.Srem
                    Syntax.Eq -> Core.Eq
        (Syntax.Lam x body) -> Core.Lam (debruijn' ctx' body)
          where ctx' = (x, 0) : incr ctx
        x@Syntax.Let {} -> debruijn' ctx (desugar x)
        (Syntax.App f arg) -> Core.App (debruijn' ctx f) (debruijn' ctx arg)
    incr :: Ctx -> Ctx
    incr ctx = zip names indices'
      where
        (names, indices) = unzip ctx
        indices' = map (+ 1) indices
    desugar :: Syntax.Expr -> Syntax.Expr
    desugar x@Syntax.Lit {} = x
    desugar x@Syntax.Var {} = x
    desugar (Syntax.Lam x body) = Syntax.Lam x (desugar body)
    desugar (Syntax.Let x e body) = (Syntax.App (Syntax.Lam x (desugar body)) e)
