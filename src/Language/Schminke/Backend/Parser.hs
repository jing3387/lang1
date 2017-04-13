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
  x <- integer
  return $ Var (fromIntegral x)

del :: Parser Expr
del = do
  reserved "del"
  x <- identifier
  return $ Del x

pop :: Parser Expr
pop = do
  reserved "pop"
  x <- identifier
  return $ Pop x

lambda :: Parser Expr
lambda = do
  reserved "lambda"
  body <- expr
  return $ Lam body

let' :: Parser Expr
let' = do
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

if' :: Parser Expr
if' = do
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
expr = parens (int <|> var <|> del <|> pop <|> lambda <|> let' <|> if' <|> app)

define :: Parser Top
define = do
  reserved "define"
  x <- identifier
  e <- expr
  return $ Def x e

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
