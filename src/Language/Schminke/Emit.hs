{-# LANGUAGE OverloadedStrings #-}

module Language.Schminke.Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Language.Schminke.Codegen
import qualified Language.Schminke.Env as Env
import qualified Language.Schminke.Syntax as S
import Language.Schminke.Type

toSig :: [Type] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (typeof x, AST.Name (show x)))

typeof :: Type -> T.Type
typeof t =
  case t of
    TVar _ -> error $ "type variable: " ++ show t
    TCon "i1" -> T.i1
    TCon "i64" -> T.i64

codegenTop :: Env.Env -> S.Top -> LLVM ()
codegenTop env (S.Def name args body) = do
  define (typeof retty) name fnargs bls
  where
    (Forall _ (TArr retty argtys)) =
      case name `Env.lookup` env of
        Nothing -> error $ "no declaration for: " ++ name
        Just sch -> sch
    fnargs = toSig argtys
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM (zip args argtys) $ \(a, t) -> do
        var <- alloca (typeof t)
        store var (local (AST.Name a))
        assign a var
      cbody <- mapM cgen body
      ret (last cbody)
codegenTop _ (S.Dec name sch) = declare (typeof retty) name fnargs
  where fnargs = toSig argtys
        (Forall _ (TArr retty argtys)) = sch

binops = Map.fromList
  [ ("add", add)
  , ("sub", sub)
  , ("mul", mul)
  , ("sdiv", sdiv)
  , ("srem", srem)
  , ("eq", eq)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.Var x) = getvar x >>= load
cgen (S.Lit (S.Int n)) = return $ cons $ C.Int 64 n
cgen (S.App binop [a, b]) | binop `Map.member` binops = do
  let f = binops Map.! binop
  ca <- cgen a
  cb <- cgen b
  f ca cb
cgen (S.App fn args) = do
  cargs <- mapM cgen args
  call (externf (AST.Name fn)) cargs

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: Env.Env -> AST.Module -> [S.Top] -> IO ()
codegen env mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context ast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
  where
    modn = mapM (codegenTop env) fns
    ast = runLLVM mod modn
