module Main (main) where

import Parser.Program (runFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Please inform the input filename. Closing application..."
    filenames -> mapM_ runFile filenames
