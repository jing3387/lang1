module Language.Schminke.Frontend.ParserSpec
  ( main
  , spec
  ) where

import qualified Data.Text.Lazy as L
import Test.Hspec
import Text.Parsec

import Language.Schminke.Frontend.Parser
import Language.Schminke.Frontend.Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseExpr" $ do
    context "when provided with valid input" $ do
      it "parses an integer" $
        parseExpr (L.pack "0") `shouldBe` (Right (Lit (Int 0)))
      it "parses a variable" $
        parseExpr (L.pack "x") `shouldBe` (Right (Var "x"))
      it "parses a lambda expression with no arguments" $
        parseExpr (L.pack "(lambda () x)") `shouldBe` (Right (Var "x"))
      it "parses a lambda expression with one argument" $
        parseExpr (L.pack "(lambda (x) x)") `shouldBe`
        (Right (Lam "x" (Var "x")))
      it "parses a lambda expression with multiple arguments" $
        parseExpr (L.pack "(lambda (x y) x)") `shouldBe`
        (Right (Lam "x" (Lam "y" (Var "x"))))
      it "parses a let expression with no bindings" $
        parseExpr (L.pack "(let () x)") `shouldBe` (Right (Var "x"))
      it "parses a let expression with one binding" $
        parseExpr (L.pack "(let ((x 0)) x)") `shouldBe`
        (Right (Let "x" (Lit (Int 0)) (Var "x")))
      it "parses a let expression with multiple bindings" $
        parseExpr (L.pack "(let ((x 0) (y x)) y)") `shouldBe`
        (Right (Let "x" (Lit (Int 0)) (Let "y" (Var "x") (Var "y"))))
      it "parses an application with no arguments" $
        parseExpr (L.pack "(f)") `shouldBe` (Right (Var "f"))
      it "parses an application with one argument" $
        parseExpr (L.pack "(f 0)") `shouldBe`
        (Right (App (Var "f") (Lit (Int 0))))
      it "parses an application with multiple arguments" $
        parseExpr (L.pack "((lambda (f x) (f x)) (lambda (x) x) 0)") `shouldBe`
        (Right
           (App
              (App
                 (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
                 (Lam "x" (Var "x")))
              (Lit (Int 0))))
    context "when provided with invalid input" $ do
      it "should not parse an empty list" $ pending
