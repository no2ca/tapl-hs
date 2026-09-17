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
