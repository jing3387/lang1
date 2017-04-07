module Language.Schminke.Frontend.Lexer where

import Control.Monad (void)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text.Lazy

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

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy (alphaNumChar <|> symbolChar) *> sc

reservedWords :: [String]
reservedWords = ["define", "lambda", "let"]

reservedSymbols :: String
reservedSymbols = "()"

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p =
      (:) <$> (letterChar <|> symbolChar) <*> many (alphaNumChar <|> symbolChar)
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

contents :: Parser a -> Parser a
contents p = between sc eof p
