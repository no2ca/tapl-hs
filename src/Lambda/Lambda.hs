module Lambda.Lambda where

data Term
  = TmVar Int
  | TmAbs Term
  | TmApp Term Term
  deriving (Eq, Show)

eval1 :: Term -> Maybe Term
-- E-App1
eval1 (TmApp t1 t2) = TmApp <$> eval1 t1 <*> pure t2
eval1 _ = Nothing
