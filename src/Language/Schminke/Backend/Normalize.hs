module Language.Schminke.Backend.Normalize
  ( normalizeTerm
  , normalizeProgram
  ) where

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core
import Language.Schminke.Backend.Sub

normalizeProgram :: Program -> Program
normalizeProgram (Program defs expr) = Program defs' expr'
  where
    defs' = map normalizeDefine defs
    expr' =
      case expr of
        Nothing -> Nothing
        Just e -> Just $ normalizeTerm 0 e

normalizeDefine :: Top -> Top
normalizeDefine (Def x e) = Def x (normalizeTerm 0 e)

normalizeTerm :: Word -> Expr -> Expr
normalizeTerm c m = normalize c m (\c x -> x)

normalize :: Word -> Expr -> (Word -> Expr -> Expr) -> Expr
normalize c m k =
  case m of
    (Lambda body) -> k c (Lambda (normalizeTerm (c + 1) body))
    (App f m') ->
      (normalizeName
         c
         f
         (\c' t -> normalizeName c' m' (\c'' t' -> (k c'' (App t t')))))
    x
      | isval x -> k c m

normalizeName :: Word -> Expr -> (Word -> Expr -> Expr) -> Expr
normalizeName c m k =
  normalize
    c
    m
    (\c' n ->
       if isval n
         then k c n
         else Let c' n (k (c' + 1) (Alpha c')))

isval :: Expr -> Bool
isval Lit {} = True
isval Var {} = True
isval Lambda {} = True
isval Delta {} = True
isval Alpha {} = True
isval _ = False
