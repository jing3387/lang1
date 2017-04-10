module Language.Schminke.Frontend.Parser
  ( expression
  , program
  , top
  , scheme
  ) where

import qualified Data.Text.Lazy as L
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text.Lazy

import Language.Schminke.Frontend.Lexer
import Language.Schminke.Frontend.Syntax as Syntax
import Language.Schminke.Frontend.Type

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
expr = int <|> var <|> parens (lambda <|> let' <|> if' <|> app)

declare :: Parser Top
declare = do
  reserved "declare"
  x <- identifier
  symbol ":"
  sc <- scheme
  return $ Dec x sc

define :: Parser Top
define = do
  reserved "define"
  x <- identifier
  e <- expr
  return $ Def x e

top :: Parser Top
top = try $ parens (declare <|> define)

modl :: Parser Program
modl = do
  tops <- many top
  e <- optional expr
  return $ Program tops e

expression :: Parser Expr
expression = between sc eof expr

program :: Parser Program
program = between sc eof modl

tint :: Parser Type
tint = do
  reserved "Int"
  return typeInt

tbool :: Parser Type
tbool = do
  reserved "Bool"
  return typeBool

tv :: Parser TVar
tv = do
  x <- tvId
  return $ TV x

tvar :: Parser Type
tvar = do
  x <- tv
  return $ TVar x

tterm :: Parser Type
tterm = parens texpr <|> tint <|> tbool <|> tvar

tops =
  [ [InfixL (TPro <$ symbol "*")]
  , [InfixL (TSum <$ symbol "+")]
  , [InfixR (TArr <$ symbol "->")]
  ]

texpr :: Parser Type
texpr = makeExprParser tterm tops

scheme :: Parser Scheme
scheme = do
  reserved "forall"
  tvs <- many tv
  symbol "."
  ty <- texpr
  return $ Forall tvs ty
