module Main (main) where

import Arith.Eval
import Arith.Parser
import Arith.Syntax
import SExpr
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  putStrLn $ replicate 16 '='
  putStrLn "[Evaluating arithmetic expression]"
  run

run :: IO ()
run = do
  s <- getLine
  case pretty . eval <$> (parseSExpr s >>= parse) of
    Right r -> putStrLn $ "-> " ++ r
    Left e -> hPutStrLn stderr e
  run
