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
  ppr _ (Int i) = parens $ text "int" <+> integer i

instance Pretty Expr where
  ppr p (Lit l) = ppr p l
  ppr p (Var n) = parens $ text "var" <+> integer (fromIntegral n)
  ppr p (Del d) = parens $ text "del" <+> ppr p d
  ppr p (Pop prim) = parens $ text "pop" <+> ppr p prim
  ppr p (Lam body) = parens $ text "lambda" $$ nest 1 (ppr p body)
  ppr p (Let x e1 e2) =
    parens $ text "let" <+> parens (ppr p x <+> ppr p e1) $$ nest 1 (ppr p e2)
  ppr p (If cond tr fl) =
    parens $ text "if" <+> ppr p cond $$ nest 3 (vcat [ppr p tr, ppr p fl])
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
