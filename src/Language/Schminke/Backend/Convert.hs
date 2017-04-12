module Language.Schminke.Backend.Convert
  ( convert
  , debruijn
  ) where

import Data.List

import Language.Schminke.Backend.Core as Core
import Language.Schminke.Frontend.Syntax as Syntax
import Language.Schminke.Frontend.Type as Type

type Ctx = [(Type.Name, Int)]

type Defs = [(Type.Name, Syntax.Expr)]

type Vars = [Type.Name]

convert :: Syntax.Program -> Core.Program
convert (Syntax.Program defs expr) =
  Core.Program
    defs'
    (case expr of
       Nothing -> Nothing
       Just expr' ->
         let deBruijnExpr = debruijn expr'
         in Just deBruijnExpr)
  where
    defs' = convertTop defs

convertTop :: [Syntax.Top] -> [Core.Top]
convertTop [] = []
convertTop ((Syntax.Def x body):rest) =
  Core.Def x (debruijn body) : convertTop rest
convertTop (_:rest) = convertTop rest

debruijn :: Syntax.Expr -> Core.Expr
debruijn = debruijn' []
  where
    debruijn' :: Ctx -> Syntax.Expr -> Core.Expr
    debruijn' ctx expr =
      case expr of
        Syntax.Lit (Syntax.Int n) -> Core.Lit (Core.Int n)
        Syntax.Var x -> x'
          where x' =
                  case lookup x ctx of
                    Nothing -> Del x
                    Just n' -> (Core.Var (fromIntegral n'))
        Syntax.Lam x body -> Core.Lam (debruijn' ctx' body)
          where ctx' = (x, 0) : incr ctx
        x@Syntax.Let {} -> debruijn' ctx (desugar x)
        x@Syntax.If {} -> debruijn' ctx (desugar x)
        Syntax.App f arg -> Core.App (debruijn' ctx f) (debruijn' ctx arg)

incr :: Ctx -> Ctx
incr ctx = zip names indices'
  where
    (names, indices) = unzip ctx
    indices' = map (+ 1) indices

desugar :: Syntax.Expr -> Syntax.Expr
desugar x@Syntax.Lit {} = x
desugar x@Syntax.Var {} = x
desugar (Syntax.Lam x body) = Syntax.Lam x (desugar body)
desugar (Syntax.Let x e body) =
  Syntax.App (Syntax.Lam x (desugar body)) (desugar e)
desugar (Syntax.If cond tr fl) = Syntax.App (Syntax.App cond tr) fl
desugar (Syntax.App e1 e2) = Syntax.App (desugar e1) (desugar e2)
