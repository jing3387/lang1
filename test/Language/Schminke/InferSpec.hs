module Language.Schminke.InferSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Data.List
import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import Test.Hspec
import Text.Megaparsec

import qualified Language.Schminke.Env as Env
import Language.Schminke.Infer
import Language.Schminke.Parser
import Language.Schminke.Pretty
import Language.Schminke.Syntax
import Language.Schminke.Type

main :: IO ()
main = hspec spec

typeof :: String -> Scheme
typeof input =
  case parse expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right x ->
      case constraintsExpr Env.init x of
        Left err -> error $ show err
        Right (_, _, _, sc) -> sc

typeofProgram :: String -> Map.Map Name Scheme
typeofProgram input =
  case parse program "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right prog ->
      case inferTop Env.init prog of
        Left err -> error $ show err
        Right Env.TypeEnv {Env.types = t} -> t

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
        typeof "0" `shouldBe` parseScheme "() i64 ()"
      it "should infer the type scheme of the `add` operation" $
        typeof "(add 1 2)" `shouldBe` parseScheme "() i64 ()"
      it "should infer the type scheme of `eq` operation" $
        typeof "(eq 0 1)" `shouldBe` parseScheme "() i1 ()"
    context "when given an ill-typed program" $ do
      it "should not infer a type scheme when there is a type error" $
        evaluate (typeof "(add (eq 1 0) 1)") `shouldThrow` anyErrorCall
      it "should not infer a type scheme when a variable is unbound" $
        evaluate (typeof "y") `shouldThrow` anyErrorCall
      it "should not infer a type scheme when given an infinite type" $
        pendingWith "Have to figure out an example now that `lambda` is gone"
  describe "inferTop" $
    it "should infer the type of the factorial function" $
      typeofProgram
      "(declare f () i64 (i64)) (define f (n) (if (eq n 0) 1 (mul n (f (sub n 1)))))" Map.!
      "f" `shouldBe`
      parseScheme "() i64 (i64)"
