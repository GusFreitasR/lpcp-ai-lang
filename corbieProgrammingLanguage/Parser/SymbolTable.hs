module Parser.SymbolTable where

import Scanner
import Parser.Types

symtable_insert :: (Token, Token) -> SymbolTable -> SymbolTable
symtable_insert symbol table =
  case symtable_lookup (fst symbol) table of
    Just _ -> fail "variable already declared"
    Nothing -> symbol : table

symtable_update :: (Token, Token) -> SymbolTable -> SymbolTable
symtable_update _ [] = fail "variable not found"
symtable_update (token1@(Token _ c1), v1) ((token2@(Token _ c2), v2) : rest) =
  if c1 == c2 then (token1, v1) : rest
  else (token2, v2) : symtable_update (token1, v1) rest

symtable_lookup :: Token -> SymbolTable -> Maybe (Token, Token)
symtable_lookup _ [] = Nothing
symtable_lookup token1@(Token _ c1) ((token2@(Token _ c2), v2) : rest) =
  if c1 == c2 then Just (token2, v2)
  else symtable_lookup token1 rest

symtable_insert_or_update :: (Token, Token) -> SymbolTable -> SymbolTable
symtable_insert_or_update symbol table =
  case symtable_lookup (fst symbol) table of
    Just _ -> symtable_update symbol table
    Nothing -> symtable_insert symbol table
