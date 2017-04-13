module Language.Schminke.Backend.Convert
  ( convert
  , convertExpr
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
         let converted = convertExpr ns expr'
         in Just converted)
  where
    defs' = convertTop ns defs
    ns = names defs

convertTop :: [Type.Name] -> [Syntax.Top] -> [Core.Top]
convertTop _ [] = []
convertTop defs ((Syntax.Def x args body):rest) =
  Core.Def x args (map (convertExpr defs') body) : convertTop defs rest
  where defs' = args ++ defs
convertTop defs (_:rest) = convertTop defs rest

convertExpr :: [Type.Name] -> Syntax.Expr -> Core.Expr
convertExpr defs expr =
  case expr of
    Syntax.Lit (Syntax.Int n) -> Core.Lit (Core.Int n)
    Syntax.Var x -> x'
      where x' =
              if x `elem` defs
                then Core.Var x
                else if x `elem` Core.prims
                        then Core.Pop x
                        else error $ "unbound variable: " ++ show x
    Syntax.Let x e1 e2 ->
      Core.Let x (convertExpr defs e1) (convertExpr defs' e2)
      where defs' = x : defs
    Syntax.If cond tr fl ->
      Core.If
        (convertExpr defs cond)
        (convertExpr defs tr)
        (convertExpr defs fl)
    Syntax.App f arg ->
      Core.App (convertExpr defs f) (convertExpr defs arg)

names :: [Syntax.Top] -> [Type.Name]
names [] = []
names ((Syntax.Def name _ _):rest) = name : names rest
names (_:rest) = names rest
