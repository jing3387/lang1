module Language.Schminke.Backend.EvalSpec
  ( main
  , spec
  ) where

import Data.Maybe
import qualified Data.Text.Lazy as L
import Test.Hspec
import Text.Megaparsec

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core
import Language.Schminke.Backend.Eval
import Language.Schminke.Frontend.Parser

evalTest :: String -> Expr
evalTest input =
  case parse program "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right p -> fromJust $ eval $ convert p

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "eval" $ do
    it "should evaluate an integer" $
      fromChurchInt (evalTest "0") `shouldBe` (Lit (Int 0))
    it "should evaluate a function" $
      evalTest "(lambda (x) x)" `shouldBe` Lam (Var 0)
    it "should evaluate a function applied to an argument" $
      fromChurchInt (evalTest "((lambda (x) x) 0)") `shouldBe` (Lit (Int 0))
    it "should evaluate a definition" $
      evalTest "TRUE" `shouldBe` evalTest "TRUE"
    it "should evaluate a definition applied to an argument" $
      evalTest "(ISZERO 0)" `shouldBe` evalTest "TRUE"
    it "should evaluate a predicate" $
      fromChurchInt (evalTest "((ISZERO 0) 0 1)") `shouldBe` (Lit (Int 0))
    it "should evaluate the successor function" $
      evalTest "(SUCC 0)" `shouldBe`
      -- Inner redexes are ignored by `eval` so the result should be in expanded
      -- form.
      evalTest "(lambda (f x) (f ((lambda (f x) x) f x)))"
    it "should evaluate functions as arguments" $
      fromChurchInt (evalTest "((lambda (f x) (f x)) (lambda (x) x) 0)") `shouldBe`
      (Lit (Int 0))
