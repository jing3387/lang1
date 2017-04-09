{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Schminke.Backend.Pretty
  ( ppexpr
  ) where

import Text.PrettyPrint

import Language.Schminke.Backend.Core

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Name where
  ppr _ x = text x

instance Pretty Literal where
  ppr _ (Int i) = text "'" <> integer i

instance Pretty Expr where
  ppr p (Var n) = integer (fromIntegral n)
  ppr p (Prim p') = ppr p p'
  ppr p (Lit l) = ppr p l
  ppr p (Lam body) = parens $ text "λ" <+> ppr p body
  ppr p (App f arg) = parens $ ppr p f <+> ppr p arg

ppexpr :: Expr -> String
ppexpr = render . ppr 0
