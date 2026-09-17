module Arith.SyntaxSpec (spec) where

import Test.Hspec

import Arith.Syntax

spec :: Spec
spec = do
  describe "isNumericVal" $ do
    it "0 is a numeric value" $
      isNumericVal TmZero `shouldBe` True

    it "succ 0 is a numeric value" $
      isNumericVal (TmSucc TmZero) `shouldBe` True

    it "succ (succ 0) is a numeric value" $
      isNumericVal (TmSucc (TmSucc TmZero)) `shouldBe` True

    it "true / false are not numeric values" $ do
      isNumericVal TmTrue `shouldBe` False
      isNumericVal TmFalse `shouldBe` False

    it "pred 0 is not a numeric value (before reduction)" $
      isNumericVal (TmPred TmZero) `shouldBe` False

    it "succ true is not a numeric value" $
      isNumericVal (TmSucc TmTrue) `shouldBe` False

    it "succ (pred 0) is not a numeric value" $
      isNumericVal (TmSucc (TmPred TmZero)) `shouldBe` False

    it "iszero 0 is not a numeric value" $
      isNumericVal (TmIsZero TmZero) `shouldBe` False

    it "if expressions are not numeric values" $
      isNumericVal (TmIf TmTrue TmZero TmZero) `shouldBe` False

  describe "isVal" $ do
    it "true / false are values" $ do
      isVal TmTrue `shouldBe` True
      isVal TmFalse `shouldBe` True

    it "numeric values are values" $ do
      isVal TmZero `shouldBe` True
      isVal (TmSucc TmZero) `shouldBe` True
      isVal (TmSucc (TmSucc TmZero)) `shouldBe` True

    it "if expressions are not values" $
      isVal (TmIf TmTrue TmTrue TmFalse) `shouldBe` False

    it "pred / iszero are not values" $ do
      isVal (TmPred TmZero) `shouldBe` False
      isVal (TmIsZero TmZero) `shouldBe` False

    it "succ true is not a value" $
      isVal (TmSucc TmTrue) `shouldBe` False

  describe "pretty" $ do
    it "prints constants" $ do
      pretty TmTrue `shouldBe` "true"
      pretty TmFalse `shouldBe` "false"
      pretty TmZero `shouldBe` "0"

    it "does not parenthesize atomic arguments" $ do
      pretty (TmSucc TmZero) `shouldBe` "succ 0"
      pretty (TmPred TmZero) `shouldBe` "pred 0"
      pretty (TmIsZero TmZero) `shouldBe` "iszero 0"
      pretty (TmSucc TmTrue) `shouldBe` "succ true"

    it "parenthesizes compound arguments" $ do
      pretty (TmSucc (TmSucc TmZero)) `shouldBe` "succ (succ 0)"
      pretty (TmPred (TmSucc TmZero)) `shouldBe` "pred (succ 0)"
      pretty (TmIsZero (TmPred (TmSucc TmZero)))
        `shouldBe` "iszero (pred (succ 0))"
      pretty (TmSucc (TmIf TmTrue TmZero TmZero))
        `shouldBe` "succ (if true then 0 else 0)"

    it "prints if without parentheses around its parts" $
      pretty (TmIf (TmIsZero TmZero) (TmSucc TmZero) TmZero)
        `shouldBe` "if iszero 0 then succ 0 else 0"

    it "prints nested if" $
      pretty (TmIf (TmIf TmTrue TmFalse TmTrue) TmZero (TmSucc TmZero))
        `shouldBe` "if if true then false else true then 0 else succ 0"
