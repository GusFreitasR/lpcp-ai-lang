module Parser.Program where

import Scanner
import Text.Parsec
import Parser.Types
import Parser.Statements
import Parser.Semantic (initialSemanticState)

program :: Parser Program
program = do
  statements <- many1 stmt
  eof
  return (Program statements)

parser :: [Token] -> IO (Either ParseError (Program, SymbolTable))
parser tokens =
  runParserT (do
    parsed <- program
    st <- getState
    return (parsed, symTable st)
  ) initialSemanticState "Parsing error!" tokens

runFile :: FilePath -> IO ()
runFile filename = do
  tokens <- getTokens filename
  result <- parser tokens
  case result of
    Left err -> print err
    Right (ast, symbols) -> do
      print ast
      print symbols
