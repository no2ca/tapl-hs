module Arith.Parser (parse) where

import Arith.Syntax (Term (..))
import SExpr (SExpr (..))

parse :: SExpr -> Either String Term
parse (Atom s)
    | s == "true" = Right TmTrue
    | s == "false" = Right TmFalse
    | s == "0" = Right TmZero
    | otherwise = Left $ "'" ++ s ++ "' is not a keyword"
parse (List [t, t1])
    | t == Atom "succ" = Right TmSucc <*> parse t1
    | t == Atom "pred" = TmPred <$> parse t1
    | t == Atom "iszero" = TmIsZero <$> parse t1
parse (List [Atom "if", t1, t2, t3]) =
    TmIf <$> parse t1 <*> parse t2 <*> parse t3
parse (List x) = Left $ "invalid expression: " ++ show x