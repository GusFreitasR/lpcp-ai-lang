module Parser.Program where

import Scanner
import Text.Parsec
import System.IO
import Parser.Types
import Parser.Statements

program :: Parser Program
program = do
  statements <- many1 stmt
  eof
  return (Program statements)

parser :: [Token] -> IO (Either ParseError (Program, SymbolTable))
parser tokens =
  runParserT (do
    parsed <- program
    symbols <- getState
    return (parsed, symbols)
  ) [] "Parsing error!" tokens

runFile :: FilePath -> IO ()
runFile filename = do
  tokens <- getTokens filename
  result <- parser tokens
  case result of
    Left err -> print err
    Right (ast, symbols) -> do
      print ast
      print symbols
