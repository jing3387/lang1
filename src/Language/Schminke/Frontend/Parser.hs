module Language.Schminke.Frontend.Parser
  ( expression
  , program
  ) where

import qualified Data.Text.Lazy as L
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy

import Language.Schminke.Frontend.Lexer
import Language.Schminke.Frontend.Syntax

int :: Parser Expr
int = do
  n <- integer
  return (Lit (Int (fromIntegral n)))

var :: Parser Expr
var = do
  x <- identifier
  return (Var x)

lambda :: Parser Expr
lambda = do
  reserved "lambda"
  formals <- parens $ many identifier
  body <- expr
  return $ foldr Lam body formals

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

app :: Parser Expr
app = do
  f <- expr
  args <- many expr
  return $ foldl App f args

expr :: Parser Expr
expr = do
  es <- some $ int <|> var <|> parens (lambda <|> let' <|> app)
  return (foldl1 App es)

define :: Parser Def
define =
  parens $ do
    reserved "define"
    x <- identifier
    e <- expr
    return $ (x, e)

val :: Parser Def
val = do
  expr <- expression
  return ("it", expr)

top :: Parser Def
top = try define <|> val

modl :: Parser [Def]
modl = many top

expression :: Parser Expr
expression = between sc eof expr

program :: Parser [Def]
program = between sc eof modl
