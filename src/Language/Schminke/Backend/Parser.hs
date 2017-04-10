module Language.Schminke.Backend.Parser
  ( coreExpr
  , coreProg
  ) where

import qualified Data.Text.Lazy as L
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core as Core
import Language.Schminke.Frontend.Parser as SyntaxParser

coreExpr :: Parser Core.Expr
coreExpr = do
  expr <- expression
  return $ debruijn expr

coreProg :: Parser Core.Program
coreProg = do
  prog <- program
  return $ convert prog
