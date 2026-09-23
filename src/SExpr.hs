module SExpr (SExpr (..), parseSExpr) where

import Data.List (List)

data SExpr
    = Atom String
    | List [SExpr]
    deriving (Show, Eq)

data Token = TLParen | TRParen | TAtom String
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize _ = []

parseSExpr :: String -> Either String SExpr
parseSExpr _ = Left []