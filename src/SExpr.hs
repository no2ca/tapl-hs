module SExpr (SExpr (..), parseSExpr) where

import Data.Char (isSpace)

data SExpr
    = Atom String
    | List [SExpr]
    deriving (Show, Eq)

data Token = TLParen | TRParen | TAtom String
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | c == '(' = TLParen : tokenize cs
    | c == ')' = TRParen : tokenize cs
    | isSpace c = tokenize cs
    | otherwise =
        -- 予約語か空白に当たるまでが名前
        let (name, rest) = break isDelimiter (c : cs)
         in TAtom name : tokenize rest
  where
    isDelimiter x = isSpace x || x == '(' || x == ')'

-- S式を1つ読み、消費しなかったトークンと一緒に返す
parseOne :: [Token] -> Either String (SExpr, [Token])
parseOne [] = Left "unexpected end of input"
parseOne (TAtom s : rest) = Right (Atom s, rest)
parseOne (TRParen : _) = Left "unexpected ')'"
parseOne (TLParen : rest) = parseList [] rest

-- '('の直後から')'までの要素を集める
parseList :: [SExpr] -> [Token] -> Either String (SExpr, [Token])
parseList acc (TRParen : rest) = Right (List (reverse acc), rest)
parseList acc ts = do
    (e, rest) <- parseOne ts
    parseList (e : acc) rest

parseSExpr :: String -> Either String SExpr
parseSExpr src = do
    (e, rest) <- parseOne (tokenize src)
    case rest of
        [] -> Right e
        _ -> Left "trailing tokens after expression"
