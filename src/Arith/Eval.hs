module Arith.Eval (
    eval1,
) where

import Arith.Syntax

-- single step evaluations
eval1 :: Term -> Maybe Term
-- E-IfTrue
eval1 (TmIf TmTrue t2 _) = Just t2
-- E-IfFalse
eval1 (TmIf TmFalse _ t3) = Just t3
-- E-PredZero
eval1 (TmPred TmZero) = Just TmZero
-- E-PredSucc
eval1 (TmPred (TmSucc nv))
    | isNumericVal nv = Just nv
-- E-IsZeroZero
eval1 (TmIsZero TmZero) = Just TmTrue
-- E-IsZeroSucc
eval1 (TmIsZero (TmSucc nv))
    | isNumericVal nv = Just TmFalse
-- E-If
eval1 (TmIf t1 t2 t3) = (\t1' -> TmIf t1' t2 t3) <$> eval1 t1
-- E-Succ
eval1 (TmSucc t1) = TmSucc <$> eval1 t1
-- E-Pred
eval1 (TmPred t1) = TmPred <$> eval1 t1
-- E-IsZero
eval1 (TmIsZero t1) = TmIsZero <$> eval1 t1
-- Value or Stuck Term
eval1 (_) = Nothing
