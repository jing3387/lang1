{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Schminke.Frontend.Pretty
  ( ppconstraint
  , ppconstraints
  , ppenv
  , ppscheme
  , ppsubst
  , ppsignature
  , pptype
  ) where

import Language.Schminke.Frontend.Env
import Language.Schminke.Frontend.Infer
import Language.Schminke.Frontend.Syntax
import Language.Schminke.Frontend.Type

import qualified Data.Map as Map
import Text.PrettyPrint

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Name where
  ppr _ x = text x

instance Pretty TVar where
  ppr _ (TV x) = text x

instance Pretty Type where
  ppr p (TArr a b) = (parensIf (isArrow a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isArrow TArr {} = True
      isArrow _ = False
  ppr p (TVar a) = ppr p a
  ppr _ (TCon a) = text a

instance Pretty Scheme where
  ppr p (Forall [] t) = ppr p t
  ppr p (Forall ts t) =
    text "∀" <+> hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t

instance Pretty Expr where
  ppr p (Var a) = ppr p a
  ppr p (App a b) = parensIf (p > 0) $ ppr (p + 1) a <+> ppr p b
  ppr p (Lam a b) = text "\\" <> ppr p a <+> text "->" <+> ppr p b
  ppr p (Let a b c) =
    text "let" <> ppr p a <+> text "=" <+> ppr p b <+> text "in" <+> ppr p c
  ppr p (Lit a) = ppr p a

instance Pretty Literal where
  ppr _ (Int i) = integer i

instance Pretty Constraint where
  ppr p (a, b) = (ppr p a) <+> text " ~ " <+> (ppr p b)

instance Pretty [Constraint] where
  ppr p cs = vcat (punctuate space (map (ppr p) cs))

instance Pretty Subst where
  ppr p (Subst s) = vcat (punctuate space (map pprSub $ Map.toList s))
    where
      pprSub (a, b) = ppr 0 a <+> text "~" <+> ppr 0 b

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["cannot unify types: \n\t", pptype a, "\nwith \n\t", pptype b]
  show (InfiniteType (TV a) b) =
    concat ["cannot construct the infinite type: ", a, " = ", pptype b]
  show (Ambigious cs) =
    concat
      [ "cannot not match expected type: '" ++
      pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n"
      | (a, b) <- cs
      ]
  show (UnboundVariable a) = "not in scope: " ++ a

ppscheme :: Scheme -> String
ppscheme = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

ppsignature :: (String, Scheme) -> String
ppsignature (a, b) = a ++ " : " ++ ppscheme b

ppenv :: Env -> [String]
ppenv (TypeEnv env) = map ppsignature $ Map.toList env

ppconstraint :: Constraint -> String
ppconstraint = render . ppr 0

ppconstraints :: [Constraint] -> String
ppconstraints = render . ppr 0

ppsubst :: Subst -> String
ppsubst = render . ppr 0
