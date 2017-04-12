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
         let deBruijnExpr = debruijn ns expr'
         in Just deBruijnExpr)
  where
    defs' = convertTop ns defs
    ns = names defs

convertTop :: [Type.Name] -> [Syntax.Top] -> [Core.Top]
convertTop _ [] = []
convertTop defs ((Syntax.Def x body):rest) =
  Core.Def x (debruijn defs body) : convertTop defs rest
convertTop defs (_:rest) = convertTop defs rest

debruijn :: [Type.Name] -> Syntax.Expr -> Core.Expr
debruijn defs e = debruijn' [] e
  where
    debruijn' :: Ctx -> Syntax.Expr -> Core.Expr
    debruijn' ctx expr =
      case expr of
        Syntax.Lit (Syntax.Int n) -> Core.Lit (Core.Int n)
        Syntax.Var x -> x'
          where x' =
                  case lookup x ctx of
                    Nothing ->
                      if x `elem` defs
                        then Del x
                        else Pop x
                    Just n' -> (Core.Var (fromIntegral n'))
        Syntax.Lam x body -> Core.Lam (debruijn' ctx' body)
          where ctx' = (x, 0) : incr ctx
        Syntax.Let x e1 e2 -> Core.Let (debruijn' ctx' e1) (debruijn' ctx' e2)
          where ctx' = (x, 0) : incr ctx
        Syntax.If cond tr fl ->
          Core.If (debruijn' ctx cond) (debruijn' ctx tr) (debruijn' ctx fl)
        Syntax.App f arg -> Core.App (debruijn' ctx f) (debruijn' ctx arg)

incr :: Ctx -> Ctx
incr ctx = zip names indices'
  where
    (names, indices) = unzip ctx
    indices' = map (+ 1) indices

names :: [Syntax.Top] -> [Type.Name]
names [] = []
names ((Syntax.Def name _):rest) = name : names rest
names (_:rest) = names rest
