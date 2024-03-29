module Language.Schminke.Parser
  ( expression
  , program
  , top
  , scheme
  , texpr
  ) where

import Control.Monad (mzero)
import qualified Data.Text.Lazy as L
import Prelude hiding (sum)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text.Lazy

import Language.Schminke.Lexer
import Language.Schminke.Syntax as Syntax
import Language.Schminke.Type

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
    some $
    parens $ do
      x <- identifier
      e <- expr
      return (x, e)
  body <- some expr
  return $ Let bindings body

if_ :: Parser Expr
if_ = do
  reserved "if"
  cond <- expr
  tr <- expr
  fl <- expr
  return $ If cond tr fl

app :: Parser Expr
app = do
  f <- identifier
  args <- many expr
  return $ App f args

expr :: Parser Expr
expr = int <|> var <|> parens (let_ <|> if_ <|> app)

declare :: Parser Top
declare = do
  reserved "declare"
  x <- identifier
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
top = parens (declare <|> define)

expression :: Parser Expr
expression = between sc eof expr

program :: Parser Program
program = between sc eof (many top)

tcon :: Parser Type
tcon = do
  x <- typeId
  return $ TCon x

tarr :: Parser Type
tarr = do
  retty <- texpr
  argtys <- parens $ many $ texpr
  return $ TArr retty argtys

tpro :: Parser Type
tpro = do
  reserved "struct"
  elemtys <- some $ texpr
  return $ foldr1 TPro elemtys

tref :: Parser Type
tref = do
  reserved "*"
  elemty <- texpr
  return $ TRef elemty

texpr :: Parser Type
texpr = tcon <|> parens (tpro <|> tref <|> tarr)

scheme :: Parser Scheme
scheme = do
  retty <- texpr
  argtys <- parens $ many $ texpr
  return $ Forall [] (TArr retty argtys)
