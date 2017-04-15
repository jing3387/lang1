{-# LANGUAGE OverloadedStrings #-}

module Language.Schminke.Emit where

import LLVM.Context
import LLVM.Module

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T

import Control.Applicative
import Control.Monad.Except
import Data.Int
import qualified Data.Map as Map
import Data.Maybe
import Data.Word

import Language.Schminke.Codegen
import Language.Schminke.Core
import qualified Language.Schminke.Env as Env
import Language.Schminke.Type

false = cons $ C.Int 1 0

true = cons $ C.Int 1 1

toSig :: [(Name, Type)] -> [(AST.Type, AST.Name)]
toSig = map (\(x, ty) -> (toLLVM ty, AST.Name x))

toLLVM :: Type -> T.Type
toLLVM t =
  case t of
    TVar _ -> error $ "type variable: " ++ show t
    TCon "i1" -> T.i1
    TCon "i32" -> T.i32
    TCon "i64" -> T.i64

typeOf :: Expr -> Type
typeOf expr =
  case expr of
    Lit (Int _) -> i 64
    Var (_, ty) -> ty
    Let _ body -> last (map typeOf body)
    -- Both branches have the same type, arbitrarily pick the first branch.
    If _ tr _ -> typeOf tr
    App (_, ty) _ -> ty

codegenTop :: Top -> LLVM ()
codegenTop (Def (name, retty) args body) =
  define (toLLVM retty) name fnargs bls
  where
    fnargs = toSig args
    bls =
      createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM_ args $ \(a, t) -> do
          var <- alloca (toLLVM t)
          store var (local (AST.Name a))
          assign a var
        cbody <- mapM cgen body
        ret (last cbody)
codegenTop (Dec name sch) = declare (toLLVM retty) name fnargs
  where
    fnargs = toSig (zip (take (length argtys) letters) argtys)
    letters = [1 ..] >>= flip replicateM ['a' .. 'z']
    (Forall _ (TArr retty argtys)) = sch

binops =
  Map.fromList
    [ ("add", add)
    , ("sub", sub)
    , ("mul", mul)
    , ("sdiv", sdiv)
    , ("srem", srem)
    , ("eq", eq)
    ]

cgen :: Expr -> Codegen AST.Operand
cgen (Var (x, _)) = getvar x >>= load
cgen (Lit (Int n)) = return $ cons $ C.Int 64 n
cgen (App (binop, _) [a, b])
  | binop `Map.member` binops = do
    let f = binops Map.! binop
    ca <- cgen a
    cb <- cgen b
    f ca cb
cgen (If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  ccond <- cgen cond
  test <- icmp IP.NE false ccond
  cbr test ifthen ifelse
  setBlock ifthen
  ctr <- cgen tr
  br ifexit
  ifthen <- getBlock
  setBlock ifelse
  cfl <- cgen fl
  br ifexit
  ifelse <- getBlock
  setBlock ifexit
  -- Both branches have the same type, arbitrarily choose the first branch.
  let ty = typeOf tr
  phi (toLLVM ty) [(ctr, ifthen), (cfl, ifelse)]
cgen (App (fn, _) args) = do
  cargs <- mapM cgen args
  call (externf (AST.Name fn)) cargs

codegen :: AST.Module -> [Top] -> AST.Module
codegen mod fns = runLLVM mod modn
  where
    modn = mapM codegenTop fns
