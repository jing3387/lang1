module Language.Schminke.Backend.Parser
  ( expression
  , program
  ) where

import qualified Data.Text.Lazy as L
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text.Lazy

import Language.Schminke.Backend.Core
import Language.Schminke.Backend.Lexer

int :: Parser Expr
int = do
  reserved "int"
  n <- integer
  return (Lit (Int (fromIntegral n)))

var :: Parser Expr
var = do
  reserved "var"
  x <- identifier
  return $ Var x

pop :: Parser Expr
pop = do
  reserved "pop"
  x <- identifier
  return $ Pop x

let_ :: Parser Expr
let_ = do
  reserved "let"
  bindings <-
    parens $
    many $
    parens $ do
      x <- identifier
      e <- expr
      return (x, e)
  body <- expr
  return $ foldr (\(ident, expr) body -> Let ident expr body) body bindings

if_ :: Parser Expr
if_ = do
  reserved "if"
  cond <- expr
  tr <- expr
  fl <- expr
  return $ If cond tr fl

app :: Parser Expr
app = do
  f <- expr
  args <- many expr
  return $ foldl App f args

expr :: Parser Expr
expr = parens (int <|> var <|> pop <|> let_ <|> if_ <|> app)

define :: Parser Top
define = do
  reserved "define"
  x <- identifier
  args <- many identifier
  body <- many expr
  return $ Def x args body

top :: Parser Top
top = try $ parens define

modl :: Parser Program
modl = do
  tops <- many top
  e <- optional expr
  return $ Program tops e

expression :: Parser Expr
expression = between sc eof expr

program :: Parser Program
program = between sc eof modl
