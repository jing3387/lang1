{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Schminke.Env
  ( Env(..)
  , empty
  , Language.Schminke.Env.init
  , lookup
  , remove
  , extend
  , extends
  , merge
  , mergeEnvs
  , singleton
  , keys
  , fromList
  , toList
  ) where

import Data.Foldable hiding (toList)
import qualified Data.Map as Map
import Data.Monoid
import Prelude hiding (lookup)

import Language.Schminke.Syntax
import Language.Schminke.Type

data Env = TypeEnv
  { types :: Map.Map Name Scheme
  } deriving (Eq, Show)

empty :: Env
empty = TypeEnv Map.empty

init :: Env
init = empty `extends` prims

extend :: Env -> (Name, Scheme) -> Env
extend env (x, s) = env {types = Map.insert x s (types env)}

remove :: Env -> Name -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(Name, Scheme)] -> Env
extends env xs = env {types = Map.union (Map.fromList xs) (types env)}

lookup :: Name -> Env -> Maybe Scheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Name -> Scheme -> Env
singleton x y = TypeEnv (Map.singleton x y)

keys :: Env -> [Name]
keys (TypeEnv env) = Map.keys env

fromList :: [(Name, Scheme)] -> Env
fromList xs = TypeEnv (Map.fromList xs)

toList :: Env -> [(Name, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Monoid Env where
  mempty = empty
  mappend = merge
