module Lambda.Lambda where

data Term
  = TmVar Int
  | TmAbs Term
  | TmApp Term Term
  deriving (Eq, Show)

eval1 :: Term -> Maybe Term
-- E-App1
eval1 (TmApp t1 t2) = (\t1' -> TmApp t1' t2) <$> eval1 t1
eval1 _ = Nothing
