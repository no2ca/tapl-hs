module Arith.Syntax (
  Term (..),
  isNumericVal,
  isVal,
  pretty,
) where

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

-- nv ::= 0 | succ nv
isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False

-- v ::= true | false | nv
isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

-- concrete syntax, e.g. "if iszero (pred (succ 0)) then succ 0 else 0"
pretty :: Term -> String
pretty TmTrue = "true"
pretty TmFalse = "false"
pretty TmZero = "0"
pretty (TmIf t1 t2 t3) =
  "if " ++ pretty t1 ++ " then " ++ pretty t2 ++ " else " ++ pretty t3
pretty (TmSucc t) = "succ " ++ prettyArg t
pretty (TmPred t) = "pred " ++ prettyArg t
pretty (TmIsZero t) = "iszero " ++ prettyArg t

-- an argument of succ/pred/iszero is parenthesized unless it is atomic
prettyArg :: Term -> String
prettyArg t
  | isAtomic t = pretty t
  | otherwise = "(" ++ pretty t ++ ")"
 where
  isAtomic :: Term -> Bool
  isAtomic TmTrue = True
  isAtomic TmFalse = True
  isAtomic TmZero = True
  isAtomic _ = False
