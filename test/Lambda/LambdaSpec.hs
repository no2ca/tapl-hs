module Lambda.LambdaSpec (spec) where

import Test.Hspec

import Lambda.Lambda

-- shorthand ------------------------------------------------------------------

-- | left-associative application: t1 @@ t2 @@ t3 == (t1 t2) t3
(@@) :: Term -> Term -> Term
(@@) = TmApp

infixl 9 @@

v :: Int -> Term
v = TmVar

lam :: Term -> Term
lam = TmAbs

-- | id = λx. x
idT :: Term
idT = lam (v 0)

-- | tru = λt. λf. t
tru :: Term
tru = lam (lam (v 1))

-- | fls = λt. λf. f
fls :: Term
fls = lam (lam (v 0))

-- | and = λb. λc. b c fls
andT :: Term
andT = lam (lam (v 1 @@ v 0 @@ fls))

-- | pair = λf. λs. λb. b f s
pair :: Term
pair = lam (lam (lam (v 0 @@ v 2 @@ v 1)))

-- | fst = λp. p tru
fstT :: Term
fstT = lam (v 0 @@ tru)

-- | snd = λp. p fls
sndT :: Term
sndT = lam (v 0 @@ fls)

-- | omega = (λx. x x) (λx. x x)
omega :: Term
omega = lam (v 0 @@ v 0) @@ lam (v 0 @@ v 0)

spec :: Spec
spec = do
  describe "isVal" $ do
    it "an abstraction is a value" $ do
      isVal idT `shouldBe` True
      isVal (lam (idT @@ idT)) `shouldBe` True

    it "a variable is not a value" $
      isVal (v 0) `shouldBe` False

    it "an application is not a value" $ do
      isVal (idT @@ idT) `shouldBe` False
      isVal (v 0 @@ idT) `shouldBe` False

  describe "termShift" $ do
    describe "variables" $ do
      it "shifts a free variable by d" $ do
        termShift 1 (v 0) `shouldBe` v 1
        termShift 2 (v 3) `shouldBe` v 5

      it "shifting by 0 changes nothing" $
        termShift 0 (lam (v 0 @@ v 1) @@ v 2) `shouldBe` lam (v 0 @@ v 1) @@ v 2

      it "shifts down with a negative d" $ do
        termShift (-1) (v 1) `shouldBe` v 0
        termShift (-2) (v 5) `shouldBe` v 3

    describe "applications" $
      it "shifts both sides" $
        termShift 1 (v 0 @@ v 1) `shouldBe` v 1 @@ v 2

    describe "abstractions (cutoff)" $ do
      it "does not shift a bound variable" $
        termShift 1 (lam (v 0)) `shouldBe` lam (v 0)

      it "shifts a free variable under a binder" $
        termShift 1 (lam (v 1)) `shouldBe` lam (v 2)

      it "raises the cutoff once per binder" $ do
        termShift 1 (lam (lam (v 1))) `shouldBe` lam (lam (v 1))
        termShift 1 (lam (lam (v 2))) `shouldBe` lam (lam (v 3))

      it "shifts down only free variables under a binder" $
        termShift (-1) (lam (v 0 @@ v 2)) `shouldBe` lam (v 0 @@ v 1)

      it "leaves closed terms unchanged" $ do
        termShift 3 tru `shouldBe` tru
        termShift 3 pair `shouldBe` pair

    describe "TAPL exercise 6.2.2" $ do
      it "shift 2 (λ.λ. 1 (0 2)) = λ.λ. 1 (0 4)" $
        termShift 2 (lam (lam (v 1 @@ (v 0 @@ v 2))))
          `shouldBe` lam (lam (v 1 @@ (v 0 @@ v 4)))

      it "shift 2 (λ. 0 1 (λ. 0 1 2)) = λ. 0 3 (λ. 0 1 4)" $
        termShift 2 (lam (v 0 @@ v 1 @@ lam (v 0 @@ v 1 @@ v 2)))
          `shouldBe` lam (v 0 @@ v 3 @@ lam (v 0 @@ v 1 @@ v 4))

  describe "termSubst" $ do
    describe "variables" $ do
      it "replaces the variable j with s" $ do
        termSubst 0 idT (v 0) `shouldBe` idT
        termSubst 2 (v 7) (v 2) `shouldBe` v 7

      it "leaves other variables as is" $ do
        termSubst 0 idT (v 1) `shouldBe` v 1
        termSubst 2 idT (v 0) `shouldBe` v 0

    describe "applications" $
      it "substitutes in both sides" $
        termSubst 0 (v 5) (v 0 @@ v 1 @@ v 0) `shouldBe` v 5 @@ v 1 @@ v 5

    describe "abstractions" $ do
      it "does not replace a bound variable" $
        termSubst 0 (v 5) (lam (v 0)) `shouldBe` lam (v 0)

      it "looks for j+1 under a binder, and shifts s by 1" $
        termSubst 0 (v 5) (lam (v 1)) `shouldBe` lam (v 6)

      it "shifts s once per binder" $
        termSubst 0 (v 5) (lam (lam (v 2))) `shouldBe` lam (lam (v 7))

      it "does not shift bound variables inside s" $
        termSubst 0 (lam (v 0 @@ v 1)) (lam (v 1))
          `shouldBe` lam (lam (v 0 @@ v 2))

    -- naming context: b = 0, a = 1
    describe "TAPL exercise 6.2.5" $ do
      it "[b -> a] (b (λx.λy.b)) = a (λx.λy.a)" $
        termSubst 0 (v 1) (v 0 @@ lam (lam (v 2)))
          `shouldBe` v 1 @@ lam (lam (v 3))

      it "[b -> a (λz.a)] (b (λx.b)) = (a (λz.a)) (λx. a (λz.a))" $
        termSubst 0 (v 1 @@ lam (v 2)) (v 0 @@ lam (v 1))
          `shouldBe` (v 1 @@ lam (v 2)) @@ lam (v 2 @@ lam (v 3))

      it "[b -> a] (λb. b a) = λb. b a" $
        termSubst 0 (v 1) (lam (v 0 @@ v 2)) `shouldBe` lam (v 0 @@ v 2)

      it "[b -> a] (λa. b a) = λa'. a a'  (no variable capture)" $
        termSubst 0 (v 1) (lam (v 1 @@ v 0)) `shouldBe` lam (v 2 @@ v 0)

  describe "termSubstTop" $ do
    it "replaces variable 0 of the body with the argument" $ do
      termSubstTop idT (v 0) `shouldBe` idT
      termSubstTop tru (v 0 @@ v 0) `shouldBe` tru @@ tru

    it "the argument itself is not shifted in the end" $
      termSubstTop (v 3) (v 0) `shouldBe` v 3

    it "shifts down the other free variables (one binder disappears)" $ do
      termSubstTop idT (v 1) `shouldBe` v 0
      termSubstTop idT (v 3) `shouldBe` v 2

    it "does not touch variables bound inside the body" $
      termSubstTop tru (lam (v 0)) `shouldBe` lam (v 0)

    it "substitutes under binders inside the body" $ do
      termSubstTop idT (lam (v 1)) `shouldBe` lam idT
      termSubstTop (v 0) (lam (v 1)) `shouldBe` lam (v 1)
      termSubstTop (v 0) (lam (lam (v 2))) `shouldBe` lam (lam (v 2))

    it "(λ. 1 0 2) (λ. 0) -> 0 (λ. 0) 1  (TAPL 6.3)" $
      termSubstTop idT (v 1 @@ v 0 @@ v 2) `shouldBe` v 0 @@ idT @@ v 1

  describe "eval1" $ do
    describe "E-AppAbs" $ do
      it "(λx. x) v -> v" $
        eval1 (idT @@ tru) `shouldBe` Just tru

      it "discards an unused argument" $
        eval1 (lam fls @@ tru) `shouldBe` Just fls

      it "substitutes under an inner binder" $
        -- (λx. λy. x) id -> λy. id
        eval1 (tru @@ idT) `shouldBe` Just (lam idT)

      it "copies the argument to every occurrence" $
        eval1 (lam (v 0 @@ v 0) @@ idT) `shouldBe` Just (idT @@ idT)

      it "handles free variables: (λ. 1 0 2) (λ. 0) -> 0 (λ. 0) 1" $
        eval1 (lam (v 1 @@ v 0 @@ v 2) @@ idT)
          `shouldBe` Just (v 0 @@ idT @@ v 1)

      it "omega reduces to itself" $
        eval1 omega `shouldBe` Just omega

    describe "E-App1" $ do
      it "reduces the function part by one step" $
        eval1 ((idT @@ tru) @@ fls) `shouldBe` Just (tru @@ fls)

      it "reduces the function part first even if the argument is reducible" $
        eval1 ((idT @@ idT) @@ (idT @@ tru))
          `shouldBe` Just (idT @@ (idT @@ tru))

    describe "E-App2" $ do
      it "reduces the argument when the function part is a value" $
        eval1 (idT @@ (idT @@ tru)) `shouldBe` Just (idT @@ tru)

      it "does not apply the function before the argument is a value (call-by-value)" $
        eval1 (lam fls @@ (idT @@ tru)) `shouldBe` Just (lam fls @@ tru)

    describe "values cannot be reduced" $ do
      it "an abstraction is a normal form" $ do
        eval1 idT `shouldBe` Nothing
        eval1 tru `shouldBe` Nothing

      it "does not reduce under a binder" $
        eval1 (lam (idT @@ idT)) `shouldBe` Nothing

    describe "stuck terms" $ do
      it "a free variable alone" $
        eval1 (v 0) `shouldBe` Nothing

      it "a free variable in function position" $
        eval1 (v 0 @@ idT) `shouldBe` Nothing

      it "a free variable as the argument (not a value)" $
        eval1 (idT @@ v 0) `shouldBe` Nothing

      it "does not reduce the argument when the function part is not a value" $
        eval1 (v 0 @@ (idT @@ idT)) `shouldBe` Nothing

  describe "eval" $ do
    it "returns a value as is" $ do
      eval idT `shouldBe` idT
      eval (lam (idT @@ idT)) `shouldBe` lam (idT @@ idT)

    it "reduces nested applications of id" $
      eval (idT @@ (idT @@ (idT @@ tru))) `shouldBe` tru

    it "stops at a stuck term" $ do
      eval (v 0 @@ idT) `shouldBe` v 0 @@ idT
      eval (idT @@ idT @@ v 0) `shouldBe` idT @@ v 0

    describe "Church booleans" $ do
      it "tru v w -> v" $
        eval (tru @@ idT @@ fls) `shouldBe` idT

      it "fls v w -> w" $
        eval (fls @@ idT @@ fls) `shouldBe` fls

      it "and tru tru -> tru" $
        eval (andT @@ tru @@ tru) `shouldBe` tru

      it "and tru fls -> fls" $
        eval (andT @@ tru @@ fls) `shouldBe` fls

      it "and fls tru -> fls" $
        eval (andT @@ fls @@ tru) `shouldBe` fls

    describe "Church pairs" $ do
      it "fst (pair v w) -> v" $
        eval (fstT @@ (pair @@ idT @@ fls)) `shouldBe` idT

      it "snd (pair v w) -> w" $
        eval (sndT @@ (pair @@ idT @@ fls)) `shouldBe` fls

  describe "evalSteps" $ do
    it "is a singleton for a value" $
      evalSteps idT `shouldBe` [idT]

    it "is a singleton for a stuck term" $
      evalSteps (v 0 @@ idT) `shouldBe` [v 0 @@ idT]

    it "lists every intermediate term in order" $
      evalSteps (idT @@ (idT @@ tru))
        `shouldBe` [idT @@ (idT @@ tru), idT @@ tru, tru]

    it "shows the steps of tru v w" $
      evalSteps (tru @@ idT @@ fls)
        `shouldBe` [tru @@ idT @@ fls, lam idT @@ fls, idT]

    it "ends with the result of eval" $ do
      let t = andT @@ tru @@ fls
      last (evalSteps t) `shouldBe` eval t

    it "is lazy enough to take a prefix of a diverging evaluation" $
      take 3 (evalSteps omega) `shouldBe` [omega, omega, omega]
