{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.Text.Lazy.IO as LIO
import System.IO
import System.Environment
import Text.Megaparsec

import Language.Schminke
import Language.Schminke.Env as Env
import Language.Schminke.Syntax as Syntax

process :: String -> IO ()
process fname = do
  contents <- LIO.readFile fname
  case parse program fname contents of
    Left err -> error $ parseErrorPretty err
    Right prog ->
      let defs = definitions prog in
      let decs = declarations prog in
      let env = Env.empty `extends` decs in
      case inferTop env defs of
        Left err -> error $ show err
        Right _ -> codegen env (emptyModule fname) prog

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> process fname >> return ()
    _ -> putStrLn "invalid arguments"
