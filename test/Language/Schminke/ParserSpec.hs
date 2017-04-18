module Language.Schminke.ParserSpec
  ( main
  , spec
  ) where

import qualified Data.Text.Lazy as L
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Language.Schminke.Parser
import Language.Schminke.Syntax
import Language.Schminke.Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "expression" $ do
    context "when provided with valid input" $ do
      it "parses an integer" $
        parse expression "" (L.pack "0") `shouldParse` Lit (Int 0)
      it "parses a variable" $
        parse expression "" (L.pack "x") `shouldParse` Var "x"
      it "parses a binary operator" $
        parse expression "" (L.pack "(add 1 2)") `shouldParse`
        App "add" [Lit (Int 1), Lit (Int 2)]
      it "parses a let expression with one binding" $
        parse expression "" (L.pack "(let ((x 0)) x)") `shouldParse`
        Let [("x", Lit (Int 0))] [Var "x"]
      it "parses a let expression with multiple bindings" $
        parse expression "" (L.pack "(let ((x 0) (y x)) y)") `shouldParse`
        Let [("x", Lit (Int 0)), ("y", Var "x")] [Var "y"]
      it "parses an if expression" $
        parse expression "" (L.pack "(if (eq 0 0) 0 1)") `shouldParse`
        If (App "eq" [Lit (Int 0), Lit (Int 0)]) (Lit (Int 0)) (Lit (Int 1))
      it "parses an application with no arguments" $
        parse expression "" (L.pack "(f)") `shouldParse` App "f" []
      it "parses an application with one argument" $
        parse expression "" (L.pack "(f 0)") `shouldParse`
        App "f" [Lit (Int 0)]
      it "parses an application with multiple arguments" $
        parse expression "" (L.pack "(f 0 1)") `shouldParse`
        App "f" [Lit (Int 0), Lit (Int 1)]
    context "when provided with invalid input" $ do
      it "should not parse an unmatched left parenthesis" $
        parse expression "" `shouldFailOn` L.pack "("
      it "should not parse an unmatched right parenthesis" $
        parse expression "" `shouldFailOn` L.pack ")"
      it "should not parse a floating-point number" $
        parse expression "" `shouldFailOn` L.pack "0.0"
      it "should not parse an empty list" $
        parse expression "" `shouldFailOn` L.pack "()"
      it "should not parse a let expression with no bindings" $
        parse expression "" `shouldFailOn` L.pack "(let () x)"
      it "should not parse an empty let body" $
        parse expression "" `shouldFailOn` L.pack "(let ((x 1)))"
      it "should not parse an empty let expression" $
        parse expression "" `shouldFailOn` L.pack "(let ())"
  describe "program" $ do
    it "should parse a declaration of an integer type" $
      parse program "" (L.pack "(declare x i64 ())") `shouldParse`
      [Dec "x" (Forall [] (TArr (TCon "i64") []))]
    it "should parse a declaration of the identity function over an integer" $
      parse program "" (L.pack "(declare id i64 (i64))") `shouldParse`
      [Dec "id" (Forall [] (TArr (TCon "i64") [TCon "i64"]))]
    it "should parse a struct type declaration" $
      parse program "" (L.pack "(declare foo i1 ((struct i2 i3)))") `shouldParse`
      [Dec "foo" (Forall [] (TArr (TCon "i1") [TPro (TCon "i2") (TCon "i3")]))]
    it "should parse a function pointer declaration" $
      parse program "" (L.pack "(declare foo (* (i1 ((struct i2 i3)))) ())") `shouldParse`
      [ Dec
          "foo"
          (Forall
             []
             (TArr (TRef (TArr (TCon "i1") [TPro (TCon "i2") (TCon "i3")])) []))
      ]
    it "should parse the factorial program" $
      parse
        program
        ""
        (L.pack
           "(declare f i64 (i64)) (define f (n) (if (eq n 0) 1 (mul n (f (sub n 1)))))") `shouldParse`
      [ Dec "f" (Forall [] (TArr (TCon "i64") [TCon "i64"]))
      , Def
          "f"
          ["n"]
          [ If
              (App "eq" [Var "n", Lit (Int 0)])
              (Lit (Int 1))
              (App "mul" [Var "n", App "f" [App "sub" [Var "n", Lit (Int 1)]]])
          ]
      ]
