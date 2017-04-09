module Language.Schminke.Backend.ANF
  ( normalizeTerm
  , normalizeDefine
  ) where

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core

normalizeDefine :: Top -> Top
normalizeDefine = undefined

normalizeTerm :: Expr -> Expr
normalizeTerm m = normalize m (\x -> x)

normalize :: Expr -> (Expr -> Expr) -> Expr
normalize m k =
  case m of
    (Lam body) -> k (Lam (normalizeTerm body))
    (App f m') ->
      (normalizeName f (\t -> normalizeName m' (\t' -> (k (App t t')))))
    x
      | isval x -> k m

normalizeName :: Expr -> (Expr -> Expr) -> Expr
normalizeName m k =
  normalize
    m
    (\n ->
       if isval n
         then k n
         else App (Lam (k (Var 0))) n)

isval :: Expr -> Bool
isval Lit {} = True
isval Var {} = True
isval Lam {} = True
isval Prim {} = True
isval _ = False
