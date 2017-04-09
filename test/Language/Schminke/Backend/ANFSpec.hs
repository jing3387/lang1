module Language.Schminke.Backend.ANFSpec
  ( main
  , spec
  ) where

import qualified Data.Text.Lazy as L
import Test.Hspec
import Text.Megaparsec

import Language.Schminke.Backend.ANF
import Language.Schminke.Backend.Core
import Language.Schminke.Backend.DeBruijn
import Language.Schminke.Frontend.Parser

normTerm :: String -> Expr
normTerm input =
  case parse expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right ast ->
      let core = debruijn ast
      in normalizeTerm core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "normalizeTerm" $ do
    it "normalizes an integer to an integer" $
      normTerm "0" `shouldBe` (Lit (Int 0))
    it "normalizes a variable to a variable" $ normTerm "x" `shouldBe` (Var 0)
    it "normalizes a binary operation to a binary operation" $
      normTerm "(add 1 2)" `shouldBe` (Binop Add (Lit (Int 1)) (Lit (Int 2)))
    it "normalizes a lambda expression to a lambda expression" $
      normTerm "(lambda (x) x)" `shouldBe` (Lam (Var 0))
    it "normalizes something a bit more complicated" $
      normTerm "((lambda (f x) (f x)) (lambda (x) x) 0)" `shouldBe`
      (App
         (Lam (App (Var 0) (Lit (Int 0))))
         (App (Lam (Lam (App (Var 1) (Var 0)))) (Lam (Var 0))))
