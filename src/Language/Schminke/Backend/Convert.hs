module Language.Schminke.Backend.Convert
  ( debruijn
  , debruijnDefine
  ) where

import Data.List

import Language.Schminke.Backend.Core as Core
import Language.Schminke.Frontend.Syntax as Syntax
import Language.Schminke.Frontend.Type as Type

type Ctx = [(Type.Name, Word)]

debruijnDefine :: Syntax.Top -> Core.Top
debruijnDefine = undefined

debruijn :: Syntax.Expr -> Core.Expr
debruijn = debruijn' []
  where
    debruijn' :: Ctx -> Syntax.Expr -> Core.Expr
    debruijn' ctx expr =
      case expr of
        (Syntax.Lit (Syntax.Int n)) -> Core.Lit (Core.Int n)
        (Syntax.Var x) -> x'
          where x' =
                  if elem x Core.prims
                    then Prim x
                    else case lookup x ctx of
                           Nothing -> (Core.Var 0)
                           Just n' -> (Core.Var n')
        (Syntax.Lam x body) -> Core.Lam (debruijn' ctx' body)
          where ctx' = (x, 0) : incr ctx
        x@Syntax.Let {} -> debruijn' ctx (desugar x)
        x@Syntax.If {} -> debruijn' ctx (desugar x)
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
    desugar (Syntax.If cond tr fl) = (Syntax.App (Syntax.App cond tr) fl)
