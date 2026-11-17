module Parser.SymbolTable where

import Scanner
import Parser.Types

enterScope :: SymbolTable -> SymbolTable
enterScope table = [] : table

exitScope :: SymbolTable -> SymbolTable
exitScope (_ : outer) = outer
exitScope [] = []

lookupInScope :: Token -> Scope -> Maybe (Token, Token)
lookupInScope _ [] = Nothing
lookupInScope token1@(Token _ c1) ((token2@(Token _ c2), v2) : rest) =
  if c1 == c2 then Just (token2, v2)
  else lookupInScope token1 rest

updateInScope :: (Token, Token) -> Scope -> Scope
updateInScope _ [] = fail "variable not found"
updateInScope (token1@(Token _ c1), v1) ((token2@(Token _ c2), v2) : rest) =
  if c1 == c2 then (token1, v1) : rest
  else (token2, v2) : updateInScope (token1, v1) rest

removeInScope :: (Token, Token) -> Scope -> Scope
removeInScope _ [] = fail "variable not found"
removeInScope (token1@(Token _ c1), v1) ((token2@(Token _ c2), v2) : rest) =
  if c1 == c2 then rest
  else (token2, v2) : removeInScope (token1, v1) rest

symtable_insert :: (Token, Token) -> SymbolTable -> SymbolTable
symtable_insert symbol [] = [[symbol]]
symtable_insert symbol (current : outer) =
  case lookupInScope (fst symbol) current of
    Just _ -> fail "variable already declared"
    Nothing -> (symbol : current) : outer

symtable_update :: (Token, Token) -> SymbolTable -> SymbolTable
symtable_update _ [] = fail "variable not found"
symtable_update symbol (current : outer) =
  case lookupInScope (fst symbol) current of
    Just _ -> updateInScope symbol current : outer
    Nothing -> current : symtable_update symbol outer

symtable_remove :: (Token, Token) -> SymbolTable -> SymbolTable
symtable_remove _ [] = fail "variable not found"
symtable_remove symbol (current : outer) =
  case lookupInScope (fst symbol) current of
    Just _ -> removeInScope symbol current : outer
    Nothing -> current : symtable_remove symbol outer

symtable_lookup :: Token -> SymbolTable -> Maybe (Token, Token)
symtable_lookup _ [] = Nothing
symtable_lookup token1 (current : outer) =
  case lookupInScope token1 current of
    Just found -> Just found
    Nothing -> symtable_lookup token1 outer
