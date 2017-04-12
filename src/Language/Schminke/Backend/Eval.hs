{-# LANGUAGE RankNTypes #-}

module Language.Schminke.Backend.Eval
  ( eval
  , toChurchInt
  , fromChurchInt
  ) where

import Data.Maybe
import qualified Data.Text.Lazy as L
import Debug.Trace
import Text.Megaparsec

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core as Core
import Language.Schminke.Backend.Reduce
import Language.Schminke.Frontend.Parser

type Ctx = [(Name, Expr)]

eval :: Program -> Maybe Expr
eval (Program _ Nothing) = Nothing
eval (Program defs (Just expr)) = Just $ expr'
  where
    ctx = (map toPair defs) ++ initCtx
    expr' = reduce ctx expr

reduce :: Ctx -> Expr -> Expr
reduce _ (Lit (Int n)) = toChurchInt (fromIntegral n)
reduce ctx (Del x) =
  case lookup x ctx of
    Nothing -> error $ "unbound variable: " ++ show x
    Just expr -> expr
reduce ctx (App (Lam t12) v2)
  | isval v2 = reduce ctx (beta v2 t12)
reduce ctx (App t1 v2)
  | isval v2 = reduce ctx (App t1' v2)
  where
    t1' = reduce ctx t1
reduce ctx (App t1 t2) = reduce ctx (App t1 t2')
  where
    t2' = reduce ctx t2
reduce _ x = x

toChurchInt :: Int -> Expr
toChurchInt n = Lam (Lam (toChurchInt' n))

toChurchInt' :: Int -> Expr
toChurchInt' n
  | n < 0 = Var 0
toChurchInt' 0 = Var 0
toChurchInt' n = (App (Var 1) (toChurchInt' (pred n)))

fromChurchInt :: Expr -> Expr
fromChurchInt (Lam (Lam x)) = Lit $ Int $ fromIntegral (fromChurchInt' x)
fromChurchInt x = x

fromChurchInt' :: Expr -> Int
fromChurchInt' (Var 0) = 0
fromChurchInt' (App (Var 1) x) = 1 + fromChurchInt' x
fromChurchInt' x = error $ "not a number: " ++ show x

parseExpr :: String -> Expr
parseExpr input =
  case parse program "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right prog -> expr
      where (Program _ (Just expr)) = convert prog

initCtx :: Ctx
initCtx =
  [ ("SUCC", parseExpr "(lambda (n f x) (f (n f x)))")
  , ("TRUE", parseExpr "(lambda (x y) x)")
  , ("FALSE", parseExpr "(lambda (x y) y)")
  , ("ISZERO", parseExpr "(lambda (n) (n (lambda (x) FALSE) TRUE))")
  ]

toPair :: Top -> (Name, Expr)
toPair (Def name expr) = (name, expr)

isval :: Expr -> Bool
isval Lam {} = True
isval _ = False
