module Language.Schminke.Backend.Normalize
  ( normalizeTerm
  , normalizeProgram
  ) where

import Control.Monad.State

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core

normalizeProgram :: Program -> Program
normalizeProgram (Program defs expr) = Program defs' expr'
  where
    defs' = map normalizeDefine defs
    expr' =
      case expr of
        Nothing -> Nothing
        Just e -> Just $ normalizeTerm 0 e

normalizeDefine :: Top -> Top
normalizeDefine (Def x args body) = Def x args (map (normalizeTerm 0) body)

normalizeTerm :: Int -> Expr -> Expr
normalizeTerm c m = normalize c m (\c' x -> x)

normalize :: Int -> Expr -> (Int -> Expr -> Expr) -> Expr
normalize c m k =
  case m of
    (Let x m1 m2) -> normalize c m1 (\c' n1 -> Let x n1 (normalize c' m2 k))
    (If m1 m2 m3) ->
      normalizeName
        c
        m1
        (\c' t -> k c' (If t (normalizeTerm c' m2) (normalizeTerm c' m3)))
    (App f m') ->
      normalizeName
        c
        f
        (\c' t -> normalizeName c' m' (\c'' t' -> (k c'' (App t t'))))
    x
      | isval x -> k c m

normalizeName :: Int -> Expr -> (Int -> Expr -> Expr) -> Expr
normalizeName c m k = do
  normalize
    c
    m
    (\c' n ->
       if isval n
         then k c' n
         else let x = show c'
              in Let x n (k (c' + 1) (Var x)))

isval :: Expr -> Bool
isval Lit {} = True
isval Var {} = True
isval Pop {} = True
isval _ = False
