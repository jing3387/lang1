{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import qualified Data.Text.Lazy.IO as TIO
import LLVM.Analysis
import LLVM.Context
import LLVM.Module
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.Megaparsec

import Language.Schminke
import Language.Schminke.Env as Env
import Language.Schminke.Syntax as Syntax
import qualified Language.Schminke.Core as Core

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

process :: String -> IO ()
process fname = do
  contents <- TIO.readFile fname
  case parse program fname contents of
    Left err -> error $ parseErrorPretty err
    Right prog ->
      let defs = definitions prog
          decs = declarations prog
          env = Env.init `extends` decs
      in case inferTop env defs of
           Left err -> error $ show err
           Right (_, prog') ->
             let defs' = Core.definitions prog'
                 decs' = Core.declarations prog'
                 -- Ignore polymorphic functions for now.
                 (_, mono) = partition isPoly defs'
                 prog'' = decs' ++ mono
                 mod = codegen (emptyModule (takeBaseName fname)) prog''
             in withContext $ \context ->
                  liftError $
                  withModuleFromAST context mod $ \m -> do
                    liftError $ verify m
                    asm <- moduleLLVMAssembly m
                    putStrLn asm

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      abs <- makeAbsolute fname
      void $ process abs
    _ -> putStrLn "invalid arguments"
