{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Text.Lazy.IO as LIO
import LLVM.Analysis
import LLVM.Context
import LLVM.Module
import System.Environment
import System.IO
import Text.Megaparsec

import Language.Schminke
import Language.Schminke.Env as Env
import Language.Schminke.Syntax as Syntax

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

process :: String -> IO ()
process fname = do
  contents <- LIO.readFile fname
  case parse program fname contents of
    Left err -> error $ parseErrorPretty err
    Right prog ->
      let defs = definitions prog
      in let decs = declarations prog
         in let env = Env.init `extends` decs
            in case inferTop env defs of
                 Left err -> error $ show err
                 Right _ ->
                   let names = map (\(Def name _ _) -> name) defs
                   in let prog' =
                            filter
                              (\x ->
                                 case x of
                                   (Dec name _) -> name `notElem` names
                                   _ -> True)
                              prog
                      in let mod = codegen env (emptyModule fname) prog'
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
    [fname] -> void $ process fname
    _ -> putStrLn "invalid arguments"
