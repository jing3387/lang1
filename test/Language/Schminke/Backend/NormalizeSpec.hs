module Language.Schminke.Backend.NormalizeSpec
  ( main
  , spec
  ) where

import qualified Data.Text.Lazy as L
import Test.Hspec
import Text.Megaparsec

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core
import Language.Schminke.Backend.Normalize
import Language.Schminke.Frontend.Parser

normTerm :: String -> Expr
normTerm input =
  case parse expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right ast ->
      let core = debruijn [] ast
      in normalizeTerm core

normDefine :: String -> Program
normDefine input =
  case parse program "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right p ->
      let core = convert p
      in normalizeProgram core

parseExpr :: String -> Expr
parseExpr input =
  case parse expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right expr -> debruijn [] expr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "normalizeTerm" $ do
    it "normalizes an integer to an integer" $
      normTerm "0" `shouldBe` parseExpr "0"
    it "normalizes a naked variable to a definition reference" $
      normTerm "x" `shouldBe` parseExpr "x"
    it "normalizes a lambda expression to a lambda expression" $
      normTerm "(lambda (x) x)" `shouldBe` parseExpr "(lambda (x) x)"
    it "normalizes an application of a function to an atomic expression" $
      normTerm "((lambda (x) x) 0)" `shouldBe` parseExpr "((lambda (x) x) 0)"
    it "normalizes a complex expression" $
      normTerm "((lambda (f x) (f x)) (lambda (x) x) 0)" `shouldBe`
      parseExpr "(let ((x ((lambda (f x) (f x)) (lambda (x) x)))) (x 0))"
  describe "normalizeProgram" $ do
    it "normalizes a recursive definition" $ pending
