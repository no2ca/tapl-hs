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

-- ---------------------------------------------------------------------------
-- モック実装 (TDD 用のスタブ)。テストを通すように自分で実装する。
-- ---------------------------------------------------------------------------

-- | 値かどうか。純粋な型無しラムダ計算では値はラムダ抽象のみ。
isVal :: Term -> Bool
isVal _ = error "TODO: isVal"

-- | d 個シフト: ↑d (t)。自由変数だけを d ずらす (TAPL 定義 6.2.1)。
termShift :: Int -> Term -> Term
termShift _ _ = error "TODO: termShift"

-- | 代入: termSubst j s t = [j ↦ s] t (TAPL 定義 6.2.4)。
termSubst :: Int -> Term -> Term -> Term
termSubst _ _ _ = error "TODO: termSubst"

-- | β簡約用の代入: termSubstTop s t = ↑-1 ([0 ↦ ↑1 (s)] t)。
termSubstTop :: Term -> Term -> Term
termSubstTop _ _ = error "TODO: termSubstTop"

-- | 正規形 (値または行き詰まり) になるまで評価する。
eval :: Term -> Term
eval _ = error "TODO: eval"

-- | 評価の各ステップを最初の項から順に並べる。
evalSteps :: Term -> [Term]
evalSteps _ = error "TODO: evalSteps"
