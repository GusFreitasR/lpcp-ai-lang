module Parser.SymbolTable where

import Scanner
import Parser.Types

initialParserState :: ParserState
initialParserState = ParserState
  { symTable     = []
  , scopeStack   = ["global"]
  , scopeCounter = 0
  , varTypes     = []
  , consts       = []
  , funcSigs     = []
  , inFunction   = False
  }

currentScopeName :: ParserState -> String
currentScopeName st = case scopeStack st of
  (s : _) -> s
  []      -> "global"

scopedName :: String -> String -> String
scopedName scope name = scope ++ "#" ++ name

scopedToken :: String -> Token -> Token
scopedToken scope (Token pos (Identifier name)) = Token pos (Identifier (scopedName scope name))
scopedToken _ tok = tok

enterNamedScope :: String -> ParserState -> ParserState
enterNamedScope name st =
  let (parent, base) = case scopeStack st of
        (s : rest) -> (s, s : rest)
        []         -> ("global", ["global"])
      nextScope = parent ++ "." ++ name
  in st { scopeStack = nextScope : base }

enterAnonScope :: String -> ParserState -> ParserState
enterAnonScope prefix st =
  let (parent, base) = case scopeStack st of
        (s : rest) -> (s, s : rest)
        []         -> ("global", ["global"])
      nextId    = scopeCounter st + 1
      nextScope = parent ++ "." ++ prefix ++ "_" ++ show nextId
  in st { scopeStack = nextScope : base, scopeCounter = nextId }

exitScope :: ParserState -> ParserState
exitScope st = case scopeStack st of
  (_ : outer@(_ : _)) -> st { scopeStack = outer }
  []                  -> st { scopeStack = ["global"] }
  _                   -> st

symtable_lookup :: Token -> SymbolTable -> Maybe (Token, Token)
symtable_lookup _ [] = Nothing
symtable_lookup token1@(Token _ c1) ((token2@(Token _ c2), v2) : rest)
  | c1 == c2  = Just (token2, v2)
  | otherwise = symtable_lookup token1 rest

symtable_insert :: (Token, Token) -> SymbolTable -> Either String SymbolTable
symtable_insert symbol table =
  case symtable_lookup (fst symbol) table of
    Just _  -> Left "variable already declared"
    Nothing -> Right (symbol : table)

symtable_update :: (Token, Token) -> SymbolTable -> Either String SymbolTable
symtable_update _ [] = Left "variable not found"
symtable_update (token1@(Token _ c1), v1) ((token2@(Token _ c2), v2) : rest)
  | c1 == c2  = Right ((token1, v1) : rest)
  | otherwise = case symtable_update (token1, v1) rest of
      Left err  -> Left err
      Right xs  -> Right ((token2, v2) : xs)

symtable_remove :: Token -> SymbolTable -> Either String SymbolTable
symtable_remove _ [] = Left "variable not found"
symtable_remove token1@(Token _ c1) ((token2@(Token _ c2), v2) : rest)
  | c1 == c2  = Right rest
  | otherwise = case symtable_remove token1 rest of
      Left err -> Left err
      Right xs -> Right ((token2, v2) : xs)

symtable_lookup_scoped :: String -> ParserState -> Maybe (String, (Token, Token))
symtable_lookup_scoped name st = go (scopeStack st)
  where
    go [] = Nothing
    go (scope : outer) =
      let key = scopedName scope name
          dummyTok = Token (AlexPn 0 0 0) (Identifier key)
      in case symtable_lookup dummyTok (symTable st) of
           Just entry -> Just (key, entry)
           Nothing    -> go outer

lookupInCurrentScope :: String -> ParserState -> Maybe (String, (Token, Token))
lookupInCurrentScope name st =
  let key = scopedName (currentScopeName st) name
      dummyTok = Token (AlexPn 0 0 0) (Identifier key)
  in case symtable_lookup dummyTok (symTable st) of
       Just entry -> Just (key, entry)
       Nothing    -> Nothing

upsertAssoc :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
upsertAssoc k v [] = [(k, v)]
upsertAssoc k v ((k', v') : rest)
  | k == k'   = (k, v) : rest
  | otherwise = (k', v') : upsertAssoc k v rest

isConstKey :: String -> ParserState -> Bool
isConstKey key st = case lookup key (consts st) of
  Just _  -> True
  Nothing -> False

isConstZero :: String -> ParserState -> Bool
isConstZero key st = case lookup key (consts st) of
  Just True -> True
  _         -> False

insertScopedSymbol :: Token -> Token -> CorbieType -> Maybe Bool -> ParserState -> Either String ParserState
insertScopedSymbol nameTok valTok typ mConstZero st =
  let scope = currentScopeName st
      key   = scopedName scope (rawTokName nameTok)
      sTok  = scopedToken scope nameTok
  in case symtable_insert (sTok, valTok) (symTable st) of
       Left err -> Left err
       Right newTbl -> Right st
         { symTable = newTbl
         , varTypes = upsertAssoc key typ (varTypes st)
         , consts   = case mConstZero of
                        Just isZero -> upsertAssoc key isZero (consts st)
                        Nothing     -> consts st
         }

updateScopedSymbol :: String -> Token -> Token -> CorbieType -> ParserState -> Either String ParserState
updateScopedSymbol key nameTok valTok typ st =
  let pos  = case nameTok of Token p _ -> p
      sTok = Token pos (Identifier key)
  in case symtable_update (sTok, valTok) (symTable st) of
       Left err -> Left err
       Right newTbl -> Right st
         { symTable = newTbl
         , varTypes = upsertAssoc key typ (varTypes st)
         }

rawTokName :: Token -> String
rawTokName (Token _ (Identifier s)) = s
rawTokName (Token _ tc)             = show tc
