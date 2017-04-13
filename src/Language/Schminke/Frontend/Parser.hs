module Language.Schminke.Frontend.Parser
  ( expression
  , program
  , top
  , scheme
  ) where

import Control.Monad (mzero)
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
expr = int <|> var <|> parens (let_ <|> if_ <|> app)

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
  name <- identifier
  args <- parens $ many identifier
  body <- many expr
  return $ Def name args body

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

tcon :: Parser Type
tcon = do
  x <- typeId
  return $ TCon x

tv :: TVar -> Parser TVar
tv x@(TV s) = do
  reserved s
  return x

tvar :: [TVar] -> Parser Type
tvar tvs = do
  x <- foldr (\x p -> x <|> p) mzero (map tv tvs)
  return $ TVar x

tterm :: [TVar] -> Parser Type
tterm tvs = parens (texpr tvs) <|> try (tvar tvs) <|> tcon

tops =
  [ [InfixL (TPro <$ symbol "*")]
  , [InfixL (TSum <$ symbol "+")]
  , [InfixR (TArr <$ symbol "->")]
  ]

texpr :: [TVar] -> Parser Type
texpr tvs = makeExprParser (tterm tvs) tops

scheme :: Parser Scheme
scheme = do
  forall <- optional $ reserved "forall"
  case forall of
    Nothing -> do
      ty <- texpr []
      return $ Forall [] ty
    _ -> do
      tvs <- many typeId
      let tvs' = map TV tvs
      symbol "."
      ty <- texpr tvs'
      return $ Forall tvs' ty
