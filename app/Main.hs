{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.List (isPrefixOf, foldl')
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import System.Console.Repline
import System.Environment
import System.Exit

import Language.Schminke
import Language.Schminke.Frontend.Env as Env

data IState = IState
  { tyctx :: Env.Env
  }

initState :: IState
initState = IState Env.empty

type Repl a = HaskelineT (StateT IState IO) a

hoistErr
  :: Show e
  => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  st <- get
  mod <- hoistErr $ parseModule "<stdin>" source
  tyctx' <- hoistErr $ inferTop (tyctx st) mod
  let st' = st {tyctx = tyctx' <> tyctx st}
  when update (put st')

showOutput :: String -> IState -> Repl ()
showOutput arg st = do
  case Env.lookup "it" (tyctx st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec True contents

typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Env.lookup arg (tyctx st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> exec False (L.pack arg)

quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

defaultMatcher
  :: MonadIO m
  => [(String, CompletionFunc m)]
defaultMatcher = [(":load", fileCompleter)]

comp
  :: (Monad m, MonadState IState m)
  => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  Env.TypeEnv ctx <- gets tyctx
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

options :: [(String, [String] -> Repl ())]
options = [("load", load), ("browse", browse), ("quit", quit), ("type", typeof)]

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState $ evalRepl "? " cmd options completer pre

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell (return ())
    [fname] -> shell (load [fname])
    _ -> putStrLn "invalid arguments"
