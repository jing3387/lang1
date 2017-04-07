module Language.Schminke.Frontend.Lexer where

import Data.Functor.Identity
import qualified Data.Text.Lazy as L
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Text.Lazy
import qualified Text.Parsec.Token as Tok

symbols :: String
symbols = "!$%&*+-./:<=>?@^_~"

reservedNames :: [String]
reservedNames = ["define", "lambda", "let"]

lexer :: Tok.GenTokenParser L.Text () Identity
lexer =
  Tok.makeTokenParser $
  Tok.LanguageDef
  { Tok.commentStart = "#|"
  , Tok.commentEnd = "|#"
  , Tok.commentLine = ";"
  , Tok.nestedComments = True
  , Tok.identStart = letter <|> oneOf symbols
  , Tok.identLetter = letter <|> oneOf symbols <|> digit
  , Tok.opStart = oneOf ""
  , Tok.opLetter = oneOf ""
  , Tok.reservedNames = reservedNames
  , Tok.reservedOpNames = []
  , Tok.caseSensitive = True
  }

integer :: Parser Integer
integer = Tok.integer lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
