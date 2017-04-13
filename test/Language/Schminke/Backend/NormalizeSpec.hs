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
import Language.Schminke.Backend.Parser as Core
import Language.Schminke.Frontend.Parser as Syntax

normTerm :: String -> Expr
normTerm input =
  case parse Syntax.expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right ast ->
      let core = convertExpr [] ast
      in normalizeTerm 0 core

normProg :: String -> Program
normProg input =
  case parse Syntax.program "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right p ->
      let core = convert p
      in normalizeProgram core

parseExpr :: String -> Expr
parseExpr input =
  case parse Core.expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right expr -> expr

parseProg :: String -> Program
parseProg input =
  case parse Core.program "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right prog -> prog

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "normalizeTerm" $ do
    it "normalizes an integer to an integer" $
      normTerm "0" `shouldBe` parseExpr "(int 0)"
    it "normalizes a lambda expression to a lambda expression" $
      normTerm "(lambda (x) x)" `shouldBe` parseExpr "(lambda (var 0))"
    it "normalizes an application of a function to an atomic expression" $
      normTerm "((lambda (x) x) 0)" `shouldBe`
      parseExpr "((lambda (var 0)) (int 0))"
    it "normalizes a complex expression" $
      normTerm "((lambda (f x) (f x)) (lambda (x) x) 0)" `shouldBe`
      parseExpr
        "(let ((0 ((lambda (lambda ((var 1) (var 0)))) (lambda (var 0))))) ((del 0) (int 0)))"
    it "normalizes a let expression" $
      normTerm "(let ((x 1) (y x)) y)" `shouldBe`
      parseExpr "(let ((x (int 1)) (y (del x))) (del y))"
    it "normalizes an if expression" $
      normTerm "(if (eq 0 1) 1 0)" `shouldBe`
      parseExpr
        "(let ((0 (((pop eq) (int 0)))) (1 (((del 0) (int 1))))) (if (del 1) (int 1) (int 0)))"
    it "normalizes an if expression where the branches contain variables" $
      normTerm "(let ((x 1) (y x)) (if (eq x y) x y))" `shouldBe`
      parseExpr
        "(let ((x (int 1)) (y (del x)) (0 (((pop eq) (del x)))) (1 (((del 0) (del y))))) (if (del 1) (del x) (del y)))"
    it
      "normalizes an if expression where the conditional is a very complex expression" $
      normTerm
        "(let ((x 1) (y x)) (if ((lambda (f x) (eq (f x) 0)) (lambda (x) x) 0) x y))" `shouldBe`
      parseExpr
        "(let ((x (int 1)) (y (del x)) (0 ((lambda (lambda (let ((0 ((var 1) (var 0))) (1 ((pop eq) (del 0)))) ((del 1) (int 0))))) (lambda (var 0)))) (1 ((del 0) (int 0)))) (if (del 1) (del x) (del y)))"
  describe "normalizeProgram" $ do
    it "normalizes a recursive definition" $
      normProg
        "(define fac (lambda (n) (if (eq n 0) 1 (mul n (fac (sub n 1)))))) (fac 5)" `shouldBe`
      parseProg
        "(define fac (lambda (let ((0 ((pop eq) (var 0))) (1 ((del 0) (int 0)))) (if (del 1) (int 1) (let ((2 ((pop mul) (var 0))) (3 ((pop sub) (var 0))) (4 ((del 3) (int 1))) (5 ((del fac) (del 4)))) ((del 2) (del 5))))))) ((del fac) (int 5))"
