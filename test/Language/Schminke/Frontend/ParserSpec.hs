module Language.Schminke.Frontend.ParserSpec
  ( main
  , spec
  ) where

import qualified Data.Text.Lazy as L
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Language.Schminke.Frontend.Parser
import Language.Schminke.Frontend.Syntax
import Language.Schminke.Frontend.Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "expression" $ do
    context "when provided with valid input" $ do
      it "parses an integer" $
        parse expression "" (L.pack "0") `shouldParse` (Lit (Int 0))
      it "parses a variable" $
        parse expression "" (L.pack "x") `shouldParse` (Var "x")
      it "parses a binary operator" $
        parse expression "" (L.pack "(add 1 2)") `shouldParse`
        (App (App (Var "add") (Lit (Int 1))) (Lit (Int 2)))
      it "parses a lambda expression with no arguments" $
        parse expression "" (L.pack "(lambda () x)") `shouldParse` (Var "x")
      it "parses a lambda expression with one argument" $
        parse expression "" (L.pack "(lambda (x) x)") `shouldParse`
        (Lam "x" (Var "x"))
      it "parses a lambda expression with multiple arguments" $
        parse expression "" (L.pack "(lambda (x y) x)") `shouldParse`
        (Lam "x" (Lam "y" (Var "x")))
      it "parses a let expression with no bindings" $
        parse expression "" (L.pack "(let () x)") `shouldParse` (Var "x")
      it "parses a let expression with one binding" $
        parse expression "" (L.pack "(let ((x 0)) x)") `shouldParse`
        (Let "x" (Lit (Int 0)) (Var "x"))
      it "parses a let expression with multiple bindings" $
        parse expression "" (L.pack "(let ((x 0) (y x)) y)") `shouldParse`
        (Let "x" (Lit (Int 0)) (Let "y" (Var "x") (Var "y")))
      it "parses an if expression" $
        parse expression "" (L.pack "(if (eq 0 0) 0 1)") `shouldParse`
        (If
           (App (App (Var "eq") (Lit (Int 0))) (Lit (Int 0)))
           (Lit (Int 0))
           (Lit (Int 1)))
      it "parses an application with no arguments" $
        parse expression "" (L.pack "(f)") `shouldParse` (Var "f")
      it "parses an application with one argument" $
        parse expression "" (L.pack "(f 0)") `shouldParse`
        (App (Var "f") (Lit (Int 0)))
      it "parses an application with multiple arguments" $
        parse expression "" (L.pack "((lambda (f x) (f x)) (lambda (x) x) 0)") `shouldParse`
        (App
           (App
              (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
              (Lam "x" (Var "x")))
           (Lit (Int 0)))
    context "when provided with invalid input" $ do
      it "should not parse an unmatched left parenthesis" $
        parse expression "" `shouldFailOn` (L.pack "(")
      it "should not parse an unmatched right parenthesis" $
        parse expression "" `shouldFailOn` (L.pack ")")
      it "should not parse a floating-point number" $
        parse expression "" `shouldFailOn` (L.pack "0.0")
      it "should not parse an empty list" $
        parse expression "" `shouldFailOn` (L.pack "()")
      it "should not parse an empty lambda expression" $
        parse expression "" `shouldFailOn` (L.pack "(lambda ())")
      it "should not parse an empty let expression" $
        parse expression "" `shouldFailOn` (L.pack "(let ())")
  describe "program" $ do
    it "should parse a declaration of an integer type" $
      parse program "" (L.pack "(declare x : Int)") `shouldParse`
      [Dec "x" (TCon "Int")]
    it "should parse a declaration of the identity function" $
      parse program "" (L.pack "(declare id : a -> a)") `shouldParse`
      [Dec "id" (TArr (TVar (TV "a")) (TVar (TV "a")))]
    it "should parse a declaration of the eq function" $
      parse program "" (L.pack "(declare eq : Int -> Int -> a + b)") `shouldParse`
      [ Dec
          "eq"
          (TArr
             (TCon "Int")
             (TArr (TCon "Int") (TSum (TVar (TV "a")) (TVar (TV "b")))))
      ]
