module Arith.EvalSpec (spec) where

import Test.Hspec

import Arith.Eval
import Arith.Syntax

-- shorthand for small numerals
one, two, three :: Term
one = TmSucc TmZero
two = TmSucc one
three = TmSucc two

spec :: Spec
spec = do
  describe "eval1" $ do
    describe "E-IfTrue / E-IfFalse" $ do
      it "if true then t2 else t3 -> t2" $
        eval1 (TmIf TmTrue TmZero one) `shouldBe` Just TmZero

      it "if false then t2 else t3 -> t3" $
        eval1 (TmIf TmFalse TmZero one) `shouldBe` Just one

      it "does not reduce the branches (t2, t3 are left as is)" $
        eval1 (TmIf TmTrue (TmPred one) (TmPred two))
          `shouldBe` Just (TmPred one)

    describe "E-If" $ do
      it "reduces the condition by one step" $
        eval1 (TmIf (TmIsZero TmZero) TmTrue TmFalse)
          `shouldBe` Just (TmIf TmTrue TmTrue TmFalse)

      it "returns Nothing when the condition is stuck" $
        eval1 (TmIf TmZero TmTrue TmFalse) `shouldBe` Nothing

    describe "E-PredZero / E-PredSucc" $ do
      it "pred 0 -> 0" $
        eval1 (TmPred TmZero) `shouldBe` Just TmZero

      it "pred (succ 0) -> 0" $
        eval1 (TmPred one) `shouldBe` Just TmZero

      it "pred (succ (succ 0)) -> succ 0" $
        eval1 (TmPred two) `shouldBe` Just one

      it "pred (succ true) is stuck since E-PredSucc does not apply" $
        eval1 (TmPred (TmSucc TmTrue)) `shouldBe` Nothing

    describe "E-Pred" $ do
      it "reduces the argument by one step" $
        eval1 (TmPred (TmPred one)) `shouldBe` Just (TmPred TmZero)

      it "pred (succ (pred 0)) reduces the inner term" $
        eval1 (TmPred (TmSucc (TmPred TmZero))) `shouldBe` Just (TmPred one)

    describe "E-IsZeroZero / E-IsZeroSucc" $ do
      it "iszero 0 -> true" $
        eval1 (TmIsZero TmZero) `shouldBe` Just TmTrue

      it "iszero (succ 0) -> false" $
        eval1 (TmIsZero one) `shouldBe` Just TmFalse

      it "iszero (succ true) is stuck" $
        eval1 (TmIsZero (TmSucc TmTrue)) `shouldBe` Nothing

    describe "E-IsZero" $ do
      it "reduces the argument by one step" $
        eval1 (TmIsZero (TmPred one)) `shouldBe` Just (TmIsZero TmZero)

    describe "E-Succ" $ do
      it "reduces the argument by one step" $
        eval1 (TmSucc (TmPred one)) `shouldBe` Just one

      it "returns Nothing when the argument is stuck" $
        eval1 (TmSucc TmTrue) `shouldBe` Nothing

    describe "values cannot be reduced" $ do
      it "true / false" $ do
        eval1 TmTrue `shouldBe` Nothing
        eval1 TmFalse `shouldBe` Nothing

      it "numeric values" $ do
        eval1 TmZero `shouldBe` Nothing
        eval1 one `shouldBe` Nothing
        eval1 two `shouldBe` Nothing

    describe "stuck terms" $ do
      it "if 0 then ... is Nothing" $
        eval1 (TmIf TmZero TmTrue TmFalse) `shouldBe` Nothing

      it "pred true is Nothing" $
        eval1 (TmPred TmTrue) `shouldBe` Nothing

      it "iszero false is Nothing" $
        eval1 (TmIsZero TmFalse) `shouldBe` Nothing

  describe "eval" $ do
    it "returns values unchanged" $ do
      eval TmTrue `shouldBe` TmTrue
      eval TmZero `shouldBe` TmZero
      eval two `shouldBe` two

    it "if iszero 0 then 0 else succ 0 -> 0" $
      eval (TmIf (TmIsZero TmZero) TmZero one) `shouldBe` TmZero

    it "pred (succ (pred 0)) -> 0" $
      eval (TmPred (TmSucc (TmPred TmZero))) `shouldBe` TmZero

    it "iszero (pred (succ 0)) -> true" $
      eval (TmIsZero (TmPred one)) `shouldBe` TmTrue

    it "succ (pred (succ (succ 0))) -> succ (succ 0)" $
      eval (TmSucc (TmPred two)) `shouldBe` two

    it "evaluates nested if correctly" $
      eval (TmIf (TmIf TmTrue TmFalse TmTrue) one two) `shouldBe` two

    it "evaluates if whose condition needs multiple steps" $
      eval (TmIf (TmIsZero (TmPred (TmPred one))) one two) `shouldBe` one

    it "returns stuck terms in their stuck form" $ do
      eval (TmIf TmZero TmTrue TmFalse) `shouldBe` TmIf TmZero TmTrue TmFalse
      eval (TmSucc TmTrue) `shouldBe` TmSucc TmTrue

    it "reduces partially before getting stuck" $ do
      -- the condition gets stuck at iszero (succ true), so the branches are untouched
      eval (TmIf (TmIsZero (TmSucc TmTrue)) (TmPred one) TmZero)
        `shouldBe` TmIf (TmIsZero (TmSucc TmTrue)) (TmPred one) TmZero
      -- the inner term is reduced before the whole term gets stuck
      eval (TmSucc (TmIsZero (TmPred one)))
        `shouldBe` TmSucc TmTrue

    it "produces a term that cannot be reduced further" $ do
      let t = TmIf (TmIsZero (TmPred one)) (TmSucc (TmPred three)) TmZero
      eval1 (eval t) `shouldBe` Nothing

  describe "evalSteps" $ do
    it "returns a singleton list for values" $ do
      evalSteps TmTrue `shouldBe` [TmTrue]
      evalSteps one `shouldBe` [one]

    it "lists the term after each step in order" $
      evalSteps (TmIf (TmIsZero TmZero) TmZero one)
        `shouldBe`
          [ TmIf (TmIsZero TmZero) TmZero one
          , TmIf TmTrue TmZero one
          , TmZero
          ]

    it "traces pred (succ (pred 0))" $
      evalSteps (TmPred (TmSucc (TmPred TmZero)))
        `shouldBe`
          [ TmPred (TmSucc (TmPred TmZero))
          , TmPred one
          , TmZero
          ]

    it "starts with the input and ends with the result of eval" $ do
      let t = TmIf (TmIsZero (TmPred one)) (TmSucc (TmPred three)) TmZero
          steps = evalSteps t
      take 1 steps `shouldBe` [t]
      last steps `shouldBe` eval t

    it "connects adjacent elements via eval1" $ do
      let t = TmIf (TmIsZero (TmPred one)) (TmSucc (TmPred three)) TmZero
          steps = evalSteps t
      zipWith (\a b -> eval1 a == Just b) steps (drop 1 steps)
        `shouldSatisfy` and

    it "returns a singleton list for stuck terms" $
      evalSteps (TmSucc TmTrue) `shouldBe` [TmSucc TmTrue]
