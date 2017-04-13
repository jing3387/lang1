module Language.Schminke.Frontend.Lexer where

import Control.Monad (void)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text.Lazy

import Language.Schminke.Frontend.Syntax
import Language.Schminke.Frontend.Type

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

identStart :: Parser Char
identStart = letterChar <|> symbolChar

identLetter :: Parser Char
identLetter = alphaNumChar <|> symbolChar

reserved :: String -> Parser ()
reserved w =
  lexeme $
  try $ do
    string w
    notFollowedBy identLetter <?> ("end of " ++ show w)

reservedWords :: [String]
reservedWords = ["declare", "define", "if", "let"]

reservedTypes :: [String]
reservedTypes = ["forall"] ++ ops

typeId :: Parser String
typeId = (lexeme . try) (p >>= check)
  where
    p = (:) <$> identStart <*> many identLetter
    check x =
      if x `elem` reservedTypes
        then fail $ show x ++ " cannot be a type variable"
        else return x

identifier :: Parser String
identifier = (lexeme . try) ((p <?> "identifier") >>= check)
  where
    p = (:) <$> identStart <*> many identLetter
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x
