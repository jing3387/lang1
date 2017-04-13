module Language.Schminke.Backend.ConvertSpec
  ( main
  , spec
  ) where

import Data.Text.Lazy as L
import Test.Hspec
import Text.Megaparsec

import Language.Schminke.Backend.Convert
import Language.Schminke.Backend.Core as Core
import Language.Schminke.Frontend.Parser
import Language.Schminke.Frontend.Syntax as Syntax

main :: IO ()
main = hspec spec

parseProgram :: String -> Syntax.Program
parseProgram input =
  case parse program "" (L.pack input) of
    Left err -> error $ parseErrorPretty err
    Right p -> p

spec :: Spec
spec = do
  describe "convertExpr" $ do
    it "should convert an integer" $
      convertExpr [] (Syntax.Lit (Syntax.Int 0)) `shouldBe` Core.Lit (Core.Int 0)
    it "should convert a let expression with one binding" $
      convertExpr [] (Syntax.Let "x" (Syntax.Lit (Syntax.Int 1)) (Syntax.Var "x")) `shouldBe`
      Core.Let "x" (Core.Lit (Core.Int 1)) (Core.Var "x")
    it
      "should convert a let expression where a later binding references an earlier binding" $
      convertExpr
        []
        (Syntax.Let
           "x"
           (Syntax.Lit (Syntax.Int 1))
           (Syntax.Let "y" (Syntax.Var "x") (Syntax.Var "x"))) `shouldBe`
      Core.Let
        "x"
        (Core.Lit (Core.Int 1))
        (Core.Let "y" (Core.Var "x") (Core.Var "x"))
    it "should convert an if expression" $
      convertExpr
        []
        (Syntax.If
           (Syntax.App
              (Syntax.App (Syntax.Var "eq") (Syntax.Lit (Syntax.Int 0)))
              (Syntax.Lit (Syntax.Int 0)))
           (Syntax.Lit (Syntax.Int 0))
           (Syntax.Lit (Syntax.Int 1))) `shouldBe`
      (Core.If
         (Core.App
            (Core.App (Core.Pop "eq") (Core.Lit (Core.Int 0)))
            (Core.Lit (Core.Int 0)))
         (Core.Lit (Core.Int 0))
         (Core.Lit (Core.Int 1)))
  describe "convert" $ do
    it "converts the factorial function" $
      pending
