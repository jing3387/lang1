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
  describe "debruijn" $ do
    it "should convert an integer to an integer" $
      debruijn (Syntax.Lit (Syntax.Int 0)) `shouldBe` Core.Lit (Core.Int 0)
    it "should convert a naked variable into a definition reference" $
      debruijn (Syntax.Var "x") `shouldBe` Core.Delta "x"
    it "should convert the identity function" $
      debruijn (Syntax.Lam "x" (Syntax.Var "x")) `shouldBe`
      (Core.Lambda (Core.Var 0))
    it "should convert the constant function" $
      debruijn (Syntax.Lam "x" (Syntax.Lam "y" (Syntax.Var "x"))) `shouldBe`
      (Core.Lambda (Core.Lambda (Core.Var 1)))
    it "should convert the composition function" $
      debruijn
        (Syntax.Lam
           "f"
           (Syntax.Lam
              "g"
              (Syntax.Lam
                 "x"
                 (Syntax.App
                    (Syntax.Var "f")
                    (Syntax.App (Syntax.Var "g") (Syntax.Var "x")))))) `shouldBe`
      (Core.Lambda
         (Core.Lambda
            (Core.Lambda
               (Core.App (Core.Var 2) (Core.App (Core.Var 1) (Core.Var 0))))))
    it "should convert a let expression with one binding" $
      debruijn (Syntax.Let "x" (Syntax.Lit (Syntax.Int 1)) (Syntax.Var "x")) `shouldBe`
      (Core.App (Core.Lambda (Core.Var 0)) (Core.Lit (Core.Int 1)))
    it
      "should convert a let expression where a later binding references an earlier binding" $
      --    (let (x 1) (let (y x) x))
      -- => ((lambda (x) ((lambda (y) x) x)) 1)
      -- => ((lambda ((lambda 1) 0)) '1)
      debruijn
        (Syntax.Let
           "x"
           (Syntax.Lit (Syntax.Int 1))
           (Syntax.Let "y" (Syntax.Var "x") (Syntax.Var "x"))) `shouldBe`
      (Core.App
         (Core.Lambda (Core.App (Core.Lambda (Core.Var 1)) (Core.Var 0)))
         (Core.Lit (Core.Int 1)))
    it "should convert an if expression" $
      debruijn
        (Syntax.If
           (Syntax.App
              (Syntax.App (Syntax.Var "eq") (Syntax.Lit (Syntax.Int 0)))
              (Syntax.Lit (Syntax.Int 0)))
           (Syntax.Lit (Syntax.Int 0))
           (Syntax.Lit (Syntax.Int 1))) `shouldBe`
      (Core.App
         (Core.App
            (Core.App
               (Core.App (Core.Delta "eq") (Core.Lit (Core.Int 0)))
               (Core.Lit (Core.Int 0)))
            (Core.Lit (Core.Int 0)))
         (Core.Lit (Core.Int 1)))
  describe "convert" $ do
    it "converts the factorial function" $
      convert
        (parseProgram
           "(define f (lambda (n) (if (eq n 0) 1 (mul n (f (sub n 1))))))") `shouldBe`
      Core.Program
        [ Core.Def
            "f"
            (Core.Lambda
               (Core.App
                  (Core.App
                     (Core.App
                        (Core.App (Core.Delta "eq") (Core.Var 0))
                        (Core.Lit (Core.Int 0)))
                     (Core.Lit (Core.Int 1)))
                  (Core.App
                     (Core.App (Core.Delta "mul") (Core.Var 0))
                     (Core.App
                        (Core.Delta "f")
                        (Core.App
                           (Core.App (Core.Delta "sub") (Core.Var 0))
                           (Core.Lit (Core.Int 1)))))))
        ]
        Nothing
