module Language.Schminke.Backend.Normalize
  ( normalizeTerm
  , normalizeProgram
  ) where

import Control.Monad.State

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core
import Language.Schminke.Backend.Reduce

normalizeProgram :: Program -> Program
normalizeProgram (Program defs expr) = Program defs' expr'
  where
    defs' = map normalizeDefine defs
    expr' =
      case expr of
        Nothing -> Nothing
        Just e -> Just $ normalizeTerm e

normalizeDefine :: Top -> Top
normalizeDefine (Def x e) = Def x (normalizeTerm e)

normalizeTerm :: Expr -> Expr
normalizeTerm m = normalize 0 m (\c x -> x)

normalize :: Int -> Expr -> (Int -> Expr -> Expr) -> Expr
normalize c m k =
  case m of
    (Lam body) -> k c (Lam (normalizeTerm body))
    (App f m') ->
      normalizeName
        c
        f
        (\c' t ->
           normalizeName
             c'
             m'
             (\c'' t' -> (k c'' (App (convt c'' t) (convt c'' t')))))
    x
      | isval x -> k c (shift c m)

convt :: Int -> Expr -> Expr
convt c e =
  case e of
    Var x
      | x < 0 -> Var (c - negate x)
    x -> x

normalizeName :: Int -> Expr -> (Int -> Expr -> Expr) -> Expr
normalizeName c m k = do
  normalize
    c
    m
    (\c' n ->
       if isval n
         then k c' n
         else App (Lam (k (c' + 1) (Var (negate c')))) n)

isval :: Expr -> Bool
isval Lit {} = True
isval Var {} = True
isval Lam {} = True
isval Del {} = True
isval _ = False
