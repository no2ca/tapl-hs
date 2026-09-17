module Main (main) where

import Arith.Eval
import Arith.Syntax

main :: IO ()
main = do
  putStrLn $ replicate 16 '='
  putStrLn "[Evaluating arithmetic expression samples]"
  mapM_ runSample samples

-- prints the evaluation trace of a term, one step per line
runSample :: Term -> IO ()
runSample t = do
  putStrLn (pretty t)
  mapM_ (putStrLn . ("  -> " ++) . pretty) (drop 1 (evalSteps t))
  putStrLn ""

samples :: [Term]
samples =
  [ TmIf (TmIsZero (TmPred (TmSucc TmZero))) (TmSucc TmZero) TmZero
  , TmPred (TmSucc (TmPred TmZero))
  , TmIf (TmIf TmTrue TmFalse TmTrue) (TmSucc TmZero) (TmSucc (TmSucc TmZero))
  , TmSucc (TmIsZero (TmPred (TmSucc TmZero)))
  ]
