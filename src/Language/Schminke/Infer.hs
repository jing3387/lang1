{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Schminke.Infer
  ( Constraint
  , TypeError(..)
  , Subst(..)
  , inferTop
  , constraintsExpr
  ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (sum)

import Language.Schminke.Env as Env
import Language.Schminke.Syntax
import Language.Schminke.Type

type Infer a = (ReaderT Env (StateT InferState (Except TypeError)) a)

data InferState = InferState
  { count :: Int
  }

initInfer :: InferState
initInfer = InferState {count = 0}

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

newtype Subst =
  Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  apply s (t1 `TSum` t2) = apply s t1 `TSum` apply s t2
  apply s (t1 `TPro` t2) = apply s t1 `TPro` apply s t2
  ftv TCon {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 `TSum` t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 `TPro` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where
      s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a =>
         Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
  = UnificationFail Type
                    Type
  | InfiniteType TVar
                 Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type]
                        [Type]

runInfer :: Env
         -> Infer (Type, [Constraint])
         -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

inferExpr :: Env -> Expr -> Either TypeError Scheme
inferExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

constraintsExpr :: Env
                -> Expr
                -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right (cs, subst, ty, sc)
          where sc = closeOver $ apply subst ty

closeOver :: Type -> Scheme
closeOver = normalize . generalize Env.empty

inEnv :: [(Name, Scheme)] -> Infer a -> Infer a
inEnv xs m = do
  env <- ask
  let scope e = foldr (\(x, sc) env' -> (remove env' x) `extend` (x, sc)) env xs
  local scope m

lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> do
      t <- instantiate s
      return t

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

infer :: Expr -> Infer (Type, [Constraint])
infer expr =
  case expr of
    Lit (Int _) -> return (i 64, [])
    Var x -> do
      t <- lookupEnv x
      return (t, [])
    App f es -> do
      t1 <- lookupEnv f
      is <- mapM infer es
      let (ts, cs) = unzip is
      tv <- fresh
      let t2 = TArr tv ts
      return (tv, concat cs ++ [(t1, t2)])
    Let bs e2s -> do
      env <- ask
      let (ns, e1s) = unzip bs
      i1s <- mapM infer e1s
      let (t1s, c1s) = unzip i1s
      let sols = map runSolve c1s
      let errs = lefts sols
      when (not (null errs)) $
        -- TODO: throw multiple errors
        throwError $ head errs
      let subs = rights sols
      let scs = map (\(sub, t1) -> generalize (apply sub env) (apply sub t1)) (zip subs t1s)
      let env' = zip ns scs
      i2s <- forM (zip subs e2s) (\(sub, e2) -> inEnv env' $ local (apply sub) (infer e2))
      let (t2s, c2s) = unzip i2s
      return (last t2s, concat c1s ++ concat c2s)
    If cond tr fl -> do
      (t1, c1) <- infer cond
      (t2, c2) <- infer tr
      (t3, c3) <- infer fl
      return (t2, c1 ++ c2 ++ c3 ++ [(t1, (i 1)), (t2, t3)])

inferTop :: Env -> [Top] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((Def name args body):rest) =
  let ty@(Forall tvs (TArr _ argtys)) =
        case Env.lookup name env of
          Nothing -> error $ "no declaration: " ++ name
          Just dec -> dec in
  let xs = zip args (map (Forall tvs) argtys) in
  let env' = env `extends` xs in
  let infers = map (inferExpr env') body in
  let errs = lefts infers in
  if not (null errs)
    then Left $ head errs
    else inferTop (extend env (name, ty)) rest
inferTop env ((Dec x ty):xs) = inferTop (env `extend` (x, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)
    fv (TVar a) = [a]
    fv (TArr a b) = fv a ++ concatMap fv b
    fv (TSum a b) = fv a ++ fv b
    fv (TPro a b) = fv a ++ fv b
    fv (TCon _) = []
    normtype (TArr a b) = TArr (normtype a) (map normtype b)
    normtype (TSum a b) = sum (normtype a) (normtype b)
    normtype (TPro a b) = TPro (normtype a) (normtype b)
    normtype (TCon a) = TCon a
    normtype (TVar a) =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) =
  Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where
    st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1:ts1) (t2:ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2
  | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany (t1 : t2) (t3 : t4)
unifies (TSum t1 t2) (TSum t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TPro t1 t2) (TPro t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2):cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind :: TVar -> Type -> Solve Subst
bind a t
  | t == TVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck
  :: Substitutable a
  => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
