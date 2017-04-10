{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Schminke.Backend.Pretty
  ( ppexpr
  , ppprog
  ) where

import Text.PrettyPrint

import Language.Schminke.Backend.Core

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Name where
  ppr _ x = text x

instance Pretty Literal where
  ppr _ (Int i) = integer i

instance Pretty Expr where
  ppr p (Lit l) = ppr p l
  ppr p (Var n) = text "λ" <> integer (fromIntegral n)
  ppr p (Alpha a) = text "α" <> integer (fromIntegral a)
  ppr p (Delta d) = ppr p d
  ppr p (Lambda body) = parens $ text "λ" $$ nest 1 (ppr p body)
  ppr p (Let x e body) =
    parens $
    text "let" <+>
    parens (text "α" <> integer (fromIntegral x) <+> ppr p e) $$
    nest 1 (ppr p body)
  ppr p (App f arg) = parens $ ppr p f <+> ppr p arg

instance Pretty Top where
  ppr p (Def name expr) =
    parens (text "define" <+> ppr p name $$ nest 1 (ppr p expr))

instance Pretty Program where
  ppr p (Program defs expr) =
    vcat (map (ppr p) defs) $$
    case expr of
      Nothing -> empty
      Just x -> ppr p x

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppprog :: Program -> String
ppprog = render . ppr 0
