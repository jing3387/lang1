module Language.Schminke.Frontend.InferSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import qualified Data.Text.Lazy as L
import Test.Hspec
import Text.Megaparsec

import qualified Language.Schminke.Frontend.Env as Env
import Language.Schminke.Frontend.Infer
import Language.Schminke.Frontend.Parser
import Language.Schminke.Frontend.Syntax
import Language.Schminke.Frontend.Type

main :: IO ()
main = hspec spec

typeof :: String -> Scheme
typeof input =
  case parse expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right x ->
      case constraintsExpr Env.empty x of
        Left err -> error $ show x
        Right (_, _, _, sc) -> sc

parseScheme :: String -> Scheme
parseScheme input =
  case parse scheme "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right sch -> sch

spec :: Spec
spec = do
  describe "constraintsExpr" $ do
    context "when given a well-typed program" $ do
      it "should infer the type scheme of an integer" $
        typeof "0" `shouldBe` parseScheme "forall. Int"
      it "should infer the type scheme of the identity function" $
        typeof "(lambda (x) x)" `shouldBe` parseScheme "forall a. a -> a"
      it "should infer the type scheme of the constant function" $
        typeof "(lambda (x y) x)" `shouldBe`
        parseScheme "forall a b. a -> b -> a"
      it "should infer the type scheme of the compose function" $
        typeof "(lambda (f g x) (f (g x)))" `shouldBe`
        parseScheme "forall a b c. (a -> b) -> (c -> a) -> c -> b"
      it
        "should infer the type scheme of the identity function when passed an integer" $
        typeof "((lambda (x) x) 0)" `shouldBe` parseScheme "forall. Int"
    context "when given an ill-typed program" $ do
      it "should not infer a type scheme when there is a type error" $
        pendingWith
          "need some primitive operations on integers before this can be tested"
      it "should not infer a type scheme when a variable is unbound" $
        evaluate (typeof "y") `shouldThrow` anyErrorCall
      it "should not infer a type scheme when given an infinite type" $
        evaluate (typeof "(lambda (x) (x x))") `shouldThrow` anyErrorCall