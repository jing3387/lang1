module Language.Schminke.Subst
  ( isPoly
  ) where

import Language.Schminke.Core
import Language.Schminke.Type

isTVar :: Type -> Bool
isTVar TVar {} = True
isTVar _ = False

isPoly :: Top -> Bool
isPoly (Def (_, retty) args _) =
  isTVar retty || foldr ((||) . isTVar) False argtys
  where
    (_, argtys) = unzip args
isPoly (Dec _ (Forall tvs _)) = not $ null tvs
