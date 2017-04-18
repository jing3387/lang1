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

typeOf :: String -> Type
typeOf input =
  case parse expression "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right x ->
      case constraintsExpr Env.init x of
        Left err -> error $ show err
        Right (_, _, _, Forall _ ty, _) -> ty

typeOfProgram :: String -> Map.Map Name Scheme
typeOfProgram input =
  case parse program "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right prog ->
      case inferTop Env.init prog of
        Left err -> error $ show err
        Right (Env.TypeEnv {Env.types = t}, _) -> t

parseType :: String -> Type
parseType input =
  case parse texpr "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right ty -> ty

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
        typeOf "0" `shouldBe` parseType "i64"
      it "should infer the type scheme of the `add` operation" $
        typeOf "(add 1 2)" `shouldBe` parseType "i64"
      it "should infer the type scheme of `eq` operation" $
        typeOf "(eq 0 1)" `shouldBe` parseType "i1"
    context "when given an ill-typed program" $ do
      it "should not infer a type scheme when there is a type error" $
        evaluate (typeOf "(add (eq 1 0) 1)") `shouldThrow` anyErrorCall
      it "should not infer a type scheme when a variable is unbound" $
        evaluate (typeOf "y") `shouldThrow` anyErrorCall
      it "should not infer a type scheme when given an infinite type" $
        pendingWith "Need to think of an example without type variables."
      it "should not infer a type scheme when there's an argument mismatch" $
        evaluate (typeOf "(mul 1)") `shouldThrow` anyErrorCall
  describe "inferTop" $ do
    it "should infer the type of the factorial function" $
      typeOfProgram
        "(declare f i64 (i64)) (define f (n) (if (eq n 0) 1 (mul n (f (sub n 1)))))" Map.!
      "f" `shouldBe`
      parseScheme "i64 (i64)"
    it "should infer the type of the the main function" $
      typeOfProgram
        "(declare f i64 (i64)) (define f (n) (if (eq n 0) 1 (mul n (f (sub n 1))))) (declare main i64 ()) (define main () (f 5))" Map.!
      "main" `shouldBe`
      parseScheme "i64 ()"
