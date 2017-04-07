module Language.Schminke.Frontend.Parser
  ( parseExpr
  , parseModule
  ) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token as Tok

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
  body <- expression
  return $ foldr Lam body formals

let' :: Parser Expr
let' = do
  reserved "let"
  bindings <-
    parens $
    many $
    parens $ do
      ident <- identifier
      expr <- expression
      return (ident, expr)
  body <- expression
  return $ foldr (\(ident, expr) body -> Let ident expr body) body bindings

app :: Parser Expr
app = do
  f <- expression
  args <- many expression
  return $ foldl App f args

expression :: Parser Expr
expression = do
  es <- many1 $ int <|> var <|> parens (lambda <|> let' <|> app)
  return (foldl1 App es)

define :: Parser Def
define =
  parens $ do
    reserved "define"
    ident <- identifier
    expr <- expression
    return $ (ident, expr)

val :: Parser Def
val = do
  expr <- expression
  return ("it", expr)

top :: Parser Def
top = try define <|> val

modl :: Parser [Def]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expression) "<stdin>" input

parseModule :: FilePath -> L.Text -> Either ParseError [Def]
parseModule fname input = parse (contents modl) fname input
