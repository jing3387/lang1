module Language.Schminke.Backend.ANFSpec
  ( main
  , spec
  ) where

import qualified Data.Text.Lazy as L
import Test.Hspec
import Text.Megaparsec

import Language.Schminke.Backend.ANF
import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core
import Language.Schminke.Frontend.Parser

normTerm :: String -> Expr
normTerm input =
  case parse expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right ast ->
      let core = debruijn ast
      in normalizeTerm core

normDefine :: String -> Top
normDefine input =
  case parse top "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right def ->
      let core = debruijnDefine def
      in normalizeDefine core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "normalizeTerm" $ do
    it "normalizes an integer to an integer" $
      normTerm "0" `shouldBe` (Lit (Int 0))
    it "normalizes a variable to a variable" $ normTerm "x" `shouldBe` (Var 0)
    it "normalizes a lambda expression to a lambda expression" $
      normTerm "(lambda (x) x)" `shouldBe` (Lam (Var 0))
    it "normalizes something a bit more complicated" $
      normTerm "((lambda (f x) (f x)) (lambda (x) x) 0)" `shouldBe`
      (App
         (Lam (App (Var 0) (Lit (Int 0))))
         (App (Lam (Lam (App (Var 1) (Var 0)))) (Lam (Var 0))))
  describe "normalizeDefine" $ do
    it "normalizes the factorial function" $
      pendingWith "need to implement recursion"
