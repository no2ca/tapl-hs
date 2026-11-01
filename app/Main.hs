module Main (main) where

import Arith.Eval
import Arith.Parser
import Arith.Syntax
import SExpr
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  putStrLn $ replicate 16 '='
  putStrLn "[Evaluating arithmetic expression samples]"
  -- mapM_ runSample samples
  run
    "(if (iszero (pred (succ 0))) true false)"

run :: String -> IO ()
run s = do
  case pretty . eval <$> (parseSExpr s >>= parse) of
    Right r -> putStrLn r
    Left e -> hPutStrLn stderr e
