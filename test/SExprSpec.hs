module SExprSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec

import SExpr

spec :: Spec
spec = do
  describe "parseSExpr" $ do
    describe "atoms" $ do
      it "reads a single atom" $ do
        parseSExpr "0" `shouldBe` Right (Atom "0")
        parseSExpr "true" `shouldBe` Right (Atom "true")

      it "reads a multi-character atom as one atom" $
        parseSExpr "iszero" `shouldBe` Right (Atom "iszero")

      it "treats any non-delimiter characters as part of an atom" $ do
        parseSExpr "foo-bar" `shouldBe` Right (Atom "foo-bar")
        parseSExpr "+" `shouldBe` Right (Atom "+")
        parseSExpr "a.b'c" `shouldBe` Right (Atom "a.b'c")

    describe "lists" $ do
      it "reads the empty list" $
        parseSExpr "()" `shouldBe` Right (List [])

      it "reads a flat list" $
        parseSExpr "(succ 0)" `shouldBe` Right (List [Atom "succ", Atom "0"])

      it "keeps the order of elements" $
        parseSExpr "(if true 0 false)"
          `shouldBe` Right (List [Atom "if", Atom "true", Atom "0", Atom "false"])

      it "reads nested lists" $
        parseSExpr "(if (iszero 0) (succ 0) false)"
          `shouldBe` Right
            ( List
                [ Atom "if"
                , List [Atom "iszero", Atom "0"]
                , List [Atom "succ", Atom "0"]
                , Atom "false"
                ]
            )

      it "reads deeply nested lists" $
        parseSExpr "(succ (succ (succ 0)))"
          `shouldBe` Right
            (List [Atom "succ", List [Atom "succ", List [Atom "succ", Atom "0"]]])

      it "reads nested empty lists" $
        parseSExpr "(() (()))" `shouldBe` Right (List [List [], List [List []]])

      it "reads a list whose first element is a list" $
        parseSExpr "((succ 0) 0)"
          `shouldBe` Right (List [List [Atom "succ", Atom "0"], Atom "0"])

    describe "delimiters and whitespace" $ do
      it "splits atoms at parentheses without spaces" $
        parseSExpr "(succ(pred 0))"
          `shouldBe` Right (List [Atom "succ", List [Atom "pred", Atom "0"]])

      it "ignores leading and trailing whitespace" $
        parseSExpr "   (succ 0)   " `shouldBe` Right (List [Atom "succ", Atom "0"])

      it "ignores whitespace just inside parentheses" $
        parseSExpr "( succ 0 )" `shouldBe` Right (List [Atom "succ", Atom "0"])

      it "treats tabs and newlines as whitespace" $
        parseSExpr "(if\n\t(iszero 0)\n\t(succ 0)\n\tfalse)\n"
          `shouldBe` parseSExpr "(if (iszero 0) (succ 0) false)"

      it "treats runs of whitespace as a single separator" $
        parseSExpr "(succ     0)" `shouldBe` Right (List [Atom "succ", Atom "0"])

    describe "errors" $ do
      it "fails on empty input" $
        parseSExpr "" `shouldSatisfy` isLeft

      it "fails on whitespace-only input" $
        parseSExpr "  \n\t " `shouldSatisfy` isLeft

      it "fails when a list is not closed" $ do
        parseSExpr "(" `shouldSatisfy` isLeft
        parseSExpr "(succ 0" `shouldSatisfy` isLeft

      it "fails when an inner list is not closed" $
        parseSExpr "(if (iszero 0) (succ 0 false)" `shouldSatisfy` isLeft

      it "fails on a leading ')'" $ do
        parseSExpr ")" `shouldSatisfy` isLeft
        parseSExpr ")(succ 0)" `shouldSatisfy` isLeft

      it "fails on tokens after an atom" $ do
        parseSExpr "0 )" `shouldSatisfy` isLeft
        parseSExpr "0 1" `shouldSatisfy` isLeft

      it "fails on an extra ')' after a list" $
        parseSExpr "(succ 0))" `shouldSatisfy` isLeft

      it "fails on multiple top-level expressions" $
        parseSExpr "(succ 0) (pred 0)" `shouldSatisfy` isLeft
