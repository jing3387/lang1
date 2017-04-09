module Language.Schminke.Frontend.Parser
  ( expression
  , program
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

binop :: BinopDef -> Parser Binop
binop (str, op, _) = do
  symbol str
  return op

binops' :: Parser Expr
binops' = do
  op <- foldl1 (<|>) (map binop Syntax.binops) <?> "operator"
  e1 <- expr
  e2 <- expr
  return $ Binop op e1 e2

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
expr = int <|> var <|> parens (binops' <|> lambda <|> let' <|> app)

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

tops = [[InfixR (TArr <$ symbol "->")]]

texpr :: Parser Type
texpr = makeExprParser tterm tops

scheme :: Parser Scheme
scheme = do
  reserved "forall"
  tvs <- many tv
  symbol "."
  ty <- texpr
  return $ Forall tvs ty
