module Main (main) where

import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Control.Applicative ((<*), (*>))
import System.Environment

-- Tabela de símbolos

type SymbolTable = [(Token, Token)]

symtable_insert :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtable_insert symbol [] = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (token1@(Token _ c1), v1) ((token2@(Token _ c2), v2):t) =
  if c1 == c2 then (token1, v1) : t
  else (token2, v2) : symtable_update (token1, v1) t

symtable_remove :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (token1@(Token _ c1), v1) ((token2@(Token _ c2), v2):t) =
  if c1 == c2 then t
  else (token2, v2) : symtable_remove (token1, v1) t

symtable_lookup :: Token -> [(Token, Token)] -> Maybe (Token, Token)
symtable_lookup _ [] = Nothing
symtable_lookup token1@(Token _ c1) ((token2@(Token _ c2), v2):t) =
  if c1 == c2 then Just (token2, v2)
  else symtable_lookup token1 t

symtable_insert_or_update :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtable_insert_or_update symbol table =
  case symtable_lookup (fst symbol) table of
    Just _  -> symtable_update symbol table
    Nothing -> symtable_insert symbol table

-- AST

data Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt
  = FuncDefStmt String [String] [Stmt]
  | ModelDefStmt String [Stmt]
  | EnumDefStmt String [String]
  | ConstDeclStmt String Expr
  | AssignStmt Assign
  | FuncCallStmt FuncCall
  | IfStmt Expr [Stmt] (Maybe [Stmt])
  | WhileStmt Expr [Stmt]
  | ForStmt Assign Expr Assign [Stmt]
  | ForInStmt String Expr [Stmt]
  | ReturnStmt (Maybe Expr)
  | PrintStmt Expr
  deriving (Eq, Show)

data Assign = AssignNode Var AssignOp Expr
  deriving (Eq, Show)

data AssignOp
  = OpAssign
  | OpPlusAssign
  | OpMinusAssign
  | OpMultAssign
  | OpDivAssign
  deriving (Eq, Show)

data Var
  = SimpleVar String
  | IndexVar Var [Slice]
  deriving (Eq, Show)

data Slice
  = SliceSingle Expr
  | SliceFromTo (Maybe Expr) (Maybe Expr)
  deriving (Eq, Show)

data FuncCall = CallFunc String [Expr]
  deriving (Eq, Show)

data Expr
  = LitExpr Literal
  | VarExpr Var
  | FuncCallExpr FuncCall
  | ListExpr [Expr]
  | ListCompExpr Expr String Expr (Maybe Expr)
  | DictExpr [(String, Expr)]
  | BinaryExpr BinOp Expr Expr
  | UnaryExpr UnOp Expr
  deriving (Eq, Show)

data BinOp
  = OpOr | OpAnd
  | OpEqual | OpNotEqual | OpLess | OpLessEqual | OpGreater | OpGreaterEqual | OpIn
  | OpAdd | OpSub | OpCat
  | OpMul | OpDiv | OpMod | OpMatMul
  | OpPow
  deriving (Eq, Show)

data UnOp
  = OpNot | OpNeg | OpPos
  deriving (Eq, Show)

data Literal
  = IntLit Int
  | FloatLit Double
  | StringLit String
  | BoolLit Bool
  deriving (Eq, Show)

-- Combinadores de tokens

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ [] = pos

type Parser a = ParsecT [Token] SymbolTable IO a

matchToken :: (TokenClass -> Bool) -> Parser Token
matchToken pred = tokenPrim show update_pos get_token where
  get_token t@(Token _ tc)
    | pred tc   = Just t
    | otherwise = Nothing

tokenName :: Token -> String
tokenName (Token _ (Identifier s)) = s
tokenName (Token _ tc)             = show tc

defToken :: Parser Token
defToken = matchToken (\tc -> case tc of Def -> True; _ -> False)

modelToken :: Parser Token
modelToken = matchToken (\tc -> case tc of Model -> True; Identifier "model" -> True; _ -> False)

enumToken :: Parser Token
enumToken = matchToken (\tc -> case tc of Enum -> True; Identifier "enum" -> True; _ -> False)

constToken :: Parser Token
constToken = matchToken (\tc -> case tc of Const -> True; Identifier "const" -> True; _ -> False)

ifToken :: Parser Token
ifToken = matchToken (\tc -> case tc of If -> True; _ -> False)

elseToken :: Parser Token
elseToken = matchToken (\tc -> case tc of Else -> True; _ -> False)

forToken :: Parser Token
forToken = matchToken (\tc -> case tc of For -> True; _ -> False)

whileToken :: Parser Token
whileToken = matchToken (\tc -> case tc of While -> True; _ -> False)

returnToken :: Parser Token
returnToken = matchToken (\tc -> case tc of Return -> True; _ -> False)

printToken :: Parser Token
printToken = matchToken (\tc -> case tc of Print -> True; Identifier "print" -> True; _ -> False)

andToken :: Parser Token
andToken = matchToken (\tc -> case tc of And -> True; _ -> False)

orToken :: Parser Token
orToken = matchToken (\tc -> case tc of Or -> True; _ -> False)

notToken :: Parser Token
notToken = matchToken (\tc -> case tc of Not -> True; Identifier "not" -> True; _ -> False)

inToken :: Parser Token
inToken = matchToken (\tc -> case tc of In -> True; Identifier "in" -> True; _ -> False)

catToken :: Parser Token
catToken = matchToken (\tc -> case tc of Cat -> True; Identifier "cat" -> True; _ -> False)

openBraceToken :: Parser Token
openBraceToken = matchToken (\tc -> case tc of OpenBrace -> True; _ -> False)

closeBraceToken :: Parser Token
closeBraceToken = matchToken (\tc -> case tc of CloseBrace -> True; _ -> False)

openParenToken :: Parser Token
openParenToken = matchToken (\tc -> case tc of OpenParen -> True; _ -> False)

closeParenToken :: Parser Token
closeParenToken = matchToken (\tc -> case tc of CloseParen -> True; _ -> False)

openBracketToken :: Parser Token
openBracketToken = matchToken (\tc -> case tc of OpenBracket -> True; _ -> False)

closeBracketToken :: Parser Token
closeBracketToken = matchToken (\tc -> case tc of CloseBracket -> True; _ -> False)

commaToken :: Parser Token
commaToken = matchToken (\tc -> case tc of Comma -> True; _ -> False)

semiColonToken :: Parser Token
semiColonToken = matchToken (\tc -> case tc of SemiColon -> True; _ -> False)

colonToken :: Parser Token
colonToken = matchToken (\tc -> case tc of Colon -> True; _ -> False)

assignToken :: Parser Token
assignToken = matchToken (\tc -> case tc of Assign -> True; _ -> False)

plusAssignToken :: Parser Token
plusAssignToken = matchToken (\tc -> case tc of PlusAssign -> True; _ -> False)

minusAssignToken :: Parser Token
minusAssignToken = matchToken (\tc -> case tc of MinusAssign -> True; _ -> False)

multAssignToken :: Parser Token
multAssignToken = matchToken (\tc -> case tc of MultAssign -> True; _ -> False)

divAssignToken :: Parser Token
divAssignToken = matchToken (\tc -> case tc of DivAssign -> True; _ -> False)

plusToken :: Parser Token
plusToken = matchToken (\tc -> case tc of Plus -> True; _ -> False)

minusToken :: Parser Token
minusToken = matchToken (\tc -> case tc of Minus -> True; _ -> False)

multiplyToken :: Parser Token
multiplyToken = matchToken (\tc -> case tc of Multiply -> True; _ -> False)

divideToken :: Parser Token
divideToken = matchToken (\tc -> case tc of Divide -> True; _ -> False)

moduloToken :: Parser Token
moduloToken = matchToken (\tc -> case tc of Modulo -> True; _ -> False)

matrixMultToken :: Parser Token
matrixMultToken = matchToken (\tc -> case tc of MatrixMult -> True; _ -> False)

powerToken :: Parser Token
powerToken = matchToken (\tc -> case tc of Power -> True; _ -> False)

caretToken :: Parser Token
caretToken = matchToken (\tc -> case tc of Caret -> True; _ -> False)

equalToken :: Parser Token
equalToken = matchToken (\tc -> case tc of Equal -> True; _ -> False)

notEqualToken :: Parser Token
notEqualToken = matchToken (\tc -> case tc of NotEqual -> True; _ -> False)

lessEqualToken :: Parser Token
lessEqualToken = matchToken (\tc -> case tc of LessEqual -> True; _ -> False)

greaterEqualToken :: Parser Token
greaterEqualToken = matchToken (\tc -> case tc of GreaterEqual -> True; _ -> False)

lessToken :: Parser Token
lessToken = matchToken (\tc -> case tc of Less -> True; _ -> False)

greaterToken :: Parser Token
greaterToken = matchToken (\tc -> case tc of Greater -> True; _ -> False)

intToken :: Parser Token
intToken = tokenPrim show update_pos get_token where
  get_token t@(Token _ (IntLiteral _)) = Just t
  get_token _ = Nothing

floatToken :: Parser Token
floatToken = tokenPrim show update_pos get_token where
  get_token t@(Token _ (FloatLiteral _)) = Just t
  get_token _ = Nothing

stringToken :: Parser Token
stringToken = tokenPrim show update_pos get_token where
  get_token t@(Token _ (StringLiteral _)) = Just t
  get_token _ = Nothing

boolToken :: Parser Token
boolToken = tokenPrim show update_pos get_token where
  get_token t@(Token _ (BoolLiteral _)) = Just t
  get_token _ = Nothing

idToken :: Parser Token
idToken = tokenPrim show update_pos get_token where
  get_token t@(Token _ (Identifier s))
    | s `notElem` [ "def", "model", "enum", "const", "if", "else", "for"
                  , "while", "return", "print", "and", "or", "not", "in"
                  , "cat", "true", "false"
                  ] = Just t
  get_token _ = Nothing

-- Regras sintáticas

-- <program> → <stmt_list>
program :: Parser Program
program = do
  stmts <- many1 stmt
  eof
  return (Program stmts)

-- <block> → "{" <stmt_list> "}" | "{" "}"
block :: Parser [Stmt]
block = do
  openBraceToken
  stmts <- many stmt
  closeBraceToken
  return stmts

-- <stmt>
stmt :: Parser Stmt
stmt = choice
  [ funcDefStmt
  , ifStmt
  , whileStmt
  , forStmt
  , try (assignStmt <* semiColonToken)
  , try (funcCallStmt <* semiColonToken)
  , printStmt <* semiColonToken
  , returnStmt <* semiColonToken
  , try (constDeclStmt <* semiColonToken)
  , modelDefStmt
  , enumDefStmt
  ]

-- <func_def> → "def" <id> "(" <param_list> ")" <block> | "def" <id> "(" ")" <block>
funcDefStmt :: Parser Stmt
funcDefStmt = do
  defTok <- defToken
  nameTok <- idToken
  openParenToken
  paramToks <- sepBy idToken commaToken
  closeParenToken
  updateState (symtable_insert (nameTok, defTok))
  mapM_ (\pt -> updateState (symtable_insert (pt, defTok))) paramToks
  body <- block
  return (FuncDefStmt (tokenName nameTok) (map tokenName paramToks) body)

-- <if_stmt> → "if" "(" <logic_expr> ")" <block> [ "else" ( <block> | <if_stmt> ) ]
ifStmt :: Parser Stmt
ifStmt = do
  ifToken
  openParenToken
  cond <- logicExpr
  closeParenToken
  thenBlk <- block
  elseBlk <- optionMaybe elsePart
  return (IfStmt cond thenBlk elseBlk)

elsePart :: Parser [Stmt]
elsePart = do
  elseToken
  (block <|> fmap (\s -> [s]) ifStmt)

-- <while_stmt> → "while" "(" <logic_expr> ")" <block>
whileStmt :: Parser Stmt
whileStmt = do
  whileToken
  openParenToken
  cond <- logicExpr
  closeParenToken
  body <- block
  return (WhileStmt cond body)

-- <for_stmt> → "for" "(" <assign> ";" <logic_expr> ";" <assign> ")" <block>
--            | "for" "(" <id> "in" <expr> ")" <block>
forStmt :: Parser Stmt
forStmt = do
  forToken
  openParenToken
  s <- try forTraditional <|> forIn
  return s

forTraditional :: Parser Stmt
forTraditional = do
  initA <- assignParser
  semiColonToken
  cond <- logicExpr
  semiColonToken
  stepA <- assignParser
  closeParenToken
  body <- block
  return (ForStmt initA cond stepA body)

forIn :: Parser Stmt
forIn = do
  varTok <- idToken
  inTok <- inToken
  src <- expr
  closeParenToken
  updateState (symtable_insert_or_update (varTok, inTok))
  body <- block
  return (ForInStmt (tokenName varTok) src body)

-- <assign> → <var> <assign_op> <expr>
assignStmt :: Parser Stmt
assignStmt = fmap AssignStmt assignParser

assignParser :: Parser Assign
assignParser = do
  (v, baseTok) <- varParser
  op <- assignOpParser
  e <- logicExpr
  tokVal <- case e of
    LitExpr (IntLit n)    -> return (Token (alexPos baseTok) (IntLiteral n))
    LitExpr (FloatLit n)  -> return (Token (alexPos baseTok) (FloatLiteral n))
    LitExpr (StringLit s) -> return (Token (alexPos baseTok) (StringLiteral s))
    LitExpr (BoolLit b)   -> return (Token (alexPos baseTok) (BoolLiteral b))
    _                     -> return (Token (alexPos baseTok) Assign)
  updateState (symtable_insert_or_update (baseTok, tokVal))
  return (AssignNode v op e)

alexPos :: Token -> AlexPosn
alexPos (Token p _) = p

assignOpParser :: Parser AssignOp
assignOpParser = choice
  [ assignToken >> return OpAssign
  , plusAssignToken >> return OpPlusAssign
  , minusAssignToken >> return OpMinusAssign
  , multAssignToken >> return OpMultAssign
  , divAssignToken >> return OpDivAssign
  ]

-- <func_call> → <id> "(" <arg_list> ")" | <id> "(" ")"
funcCallStmt :: Parser Stmt
funcCallStmt = fmap FuncCallStmt funcCallParser

funcCallParser :: Parser FuncCall
funcCallParser = do
  nameTok <- idToken
  openParenToken
  args <- sepBy logicExpr commaToken
  closeParenToken
  return (CallFunc (tokenName nameTok) args)

-- <return_stmt> → "return" <expr> | "return"
returnStmt :: Parser Stmt
returnStmt = do
  returnToken
  e <- optionMaybe logicExpr
  return (ReturnStmt e)

-- <print_stmt> → "print" "(" <expr> ")"
printStmt :: Parser Stmt
printStmt = do
  printToken
  openParenToken
  e <- logicExpr
  closeParenToken
  return (PrintStmt e)

-- <const_decl> → "const" <id> "=" <const_val>
-- <const_val> → <literal> | <id>
constDeclStmt :: Parser Stmt
constDeclStmt = do
  constToken
  nameTok <- idToken
  assignToken
  (e, valTok) <- constValParser
  updateState (symtable_insert (nameTok, valTok))
  return (ConstDeclStmt (tokenName nameTok) e)

constValParser :: Parser (Expr, Token)
constValParser = choice
  [ do t@(Token _ (IntLiteral n)) <- intToken; return (LitExpr (IntLit n), t)
  , do t@(Token _ (FloatLiteral n)) <- floatToken; return (LitExpr (FloatLit n), t)
  , do t@(Token _ (StringLiteral s)) <- stringToken; return (LitExpr (StringLit s), t)
  , do t@(Token _ (BoolLiteral b)) <- boolToken; return (LitExpr (BoolLit b), t)
  , do t <- idToken; return (VarExpr (SimpleVar (tokenName t)), t)
  ]

-- <model_def> → "model" <id> <block>
modelDefStmt :: Parser Stmt
modelDefStmt = do
  modelTok <- modelToken
  nameTok <- idToken
  body <- block
  updateState (symtable_insert (nameTok, modelTok))
  return (ModelDefStmt (tokenName nameTok) body)

-- <enum_def> → "enum" <id> "{" <id_list> "}"
-- <id_list> → <id> | <id> "," <id_list>
enumDefStmt :: Parser Stmt
enumDefStmt = do
  enumTok <- enumToken
  nameTok <- idToken
  openBraceToken
  memberToks <- sepBy1 idToken commaToken
  closeBraceToken
  updateState (symtable_insert (nameTok, enumTok))
  mapM_ (\mt -> updateState (symtable_insert (mt, enumTok))) memberToks
  return (EnumDefStmt (tokenName nameTok) (map tokenName memberToks))

-- <var> → <id> | <var> "[" <index_list> "]"
varParser :: Parser (Var, Token)
varParser = do
  baseTok <- idToken
  suffixes <- many varIndexSuffix
  let v = foldl IndexVar (SimpleVar (tokenName baseTok)) suffixes
  return (v, baseTok)

varIndexSuffix :: Parser [Slice]
varIndexSuffix = do
  openBracketToken
  slices <- sepBy1 sliceParser commaToken
  closeBracketToken
  return slices

-- <slice> → <expr> ":" <expr> | <expr> ":" | ":" <expr> | ":" | <expr>
sliceParser :: Parser Slice
sliceParser = choice
  [ try sliceBoth
  , try sliceFrom
  , try sliceTo
  , try sliceAll
  , sliceIndex
  ]

sliceBoth :: Parser Slice
sliceBoth = do
  e1 <- expr
  colonToken
  e2 <- expr
  return (SliceFromTo (Just e1) (Just e2))

sliceFrom :: Parser Slice
sliceFrom = do
  e1 <- expr
  colonToken
  return (SliceFromTo (Just e1) Nothing)

sliceTo :: Parser Slice
sliceTo = do
  colonToken
  e2 <- expr
  return (SliceFromTo Nothing (Just e2))

sliceAll :: Parser Slice
sliceAll = do
  colonToken
  return (SliceFromTo Nothing Nothing)

sliceIndex :: Parser Slice
sliceIndex = fmap SliceSingle expr

-- Expressões

-- <logic_expr> → <logic_expr> "or" <logic_term> | <logic_term>
logicExpr :: Parser Expr
logicExpr = chainl1 logicTerm orOp where
  orOp = orToken >> return (BinaryExpr OpOr)

-- <logic_term> → <logic_term> "and" <rel_expr> | <rel_expr>
logicTerm :: Parser Expr
logicTerm = chainl1 relExpr andOp where
  andOp = andToken >> return (BinaryExpr OpAnd)

-- <rel_expr> → "not" <rel_expr> | <expr> <rel_op> <expr> | <expr>
relExpr :: Parser Expr
relExpr = choice
  [ do notToken; e <- relExpr; return (UnaryExpr OpNot e)
  , try (do e1 <- expr; op <- relOp; e2 <- expr; return (BinaryExpr op e1 e2))
  , expr
  ]

relOp :: Parser BinOp
relOp = choice
  [ equalToken >> return OpEqual
  , notEqualToken >> return OpNotEqual
  , lessEqualToken >> return OpLessEqual
  , greaterEqualToken >> return OpGreaterEqual
  , lessToken >> return OpLess
  , greaterToken >> return OpGreater
  , inToken >> return OpIn
  ]

-- <expr> → <expr> "+" <term> | <expr> "-" <term> | <expr> "cat" <term> | <term>
expr :: Parser Expr
expr = chainl1 term addSubCatOp where
  addSubCatOp = choice
    [ plusToken >> return (BinaryExpr OpAdd)
    , minusToken >> return (BinaryExpr OpSub)
    , catToken >> return (BinaryExpr OpCat)
    ]

-- <term> → <term> "*" <factor> | <term> "/" <factor> | <term> "%" <factor> | <term> "@" <factor> | <factor>
term :: Parser Expr
term = chainl1 factor mulDivModMatOp where
  mulDivModMatOp = choice
    [ multiplyToken >> return (BinaryExpr OpMul)
    , divideToken >> return (BinaryExpr OpDiv)
    , moduloToken >> return (BinaryExpr OpMod)
    , matrixMultToken >> return (BinaryExpr OpMatMul)
    ]

-- <factor> → <unary> "**" <factor> | <unary> "^" <factor> | <unary>
factor :: Parser Expr
factor = do
  u <- unary
  choice
    [ do powerToken; f <- factor; return (BinaryExpr OpPow u f)
    , do caretToken; f <- factor; return (BinaryExpr OpPow u f)
    , return u
    ]

-- <unary> → "-" <unary> | "+" <unary> | <primary>
unary :: Parser Expr
unary = choice
  [ minusToken >> fmap (UnaryExpr OpNeg) unary
  , plusToken >> fmap (UnaryExpr OpPos) unary
  , primary
  ]

-- <primary> → <var> | <literal> | <func_call> | <list> | <list_comp> | <dict> | "(" <expr> ")"
primary :: Parser Expr
primary = choice
  [ fmap (LitExpr . IntLit . extractInt) intToken
  , fmap (LitExpr . FloatLit . extractFloat) floatToken
  , fmap (LitExpr . StringLit . extractString) stringToken
  , fmap (LitExpr . BoolLit . extractBool) boolToken
  , between openParenToken closeParenToken logicExpr
  , listOrListComp
  , dictParser
  , try (fmap FuncCallExpr funcCallParser)
  , fmap (VarExpr . fst) varParser
  ]

extractInt :: Token -> Int
extractInt (Token _ (IntLiteral n)) = n
extractInt _ = 0

extractFloat :: Token -> Double
extractFloat (Token _ (FloatLiteral n)) = n
extractFloat _ = 0.0

extractString :: Token -> String
extractString (Token _ (StringLiteral s)) = s
extractString _ = ""

extractBool :: Token -> Bool
extractBool (Token _ (BoolLiteral b)) = b
extractBool _ = False

-- <list> → "[" <arg_list> "]" | "[" "]"
-- <list_comp> → "[" <expr> "for" <id> "in" <expr> [ "if" <logic_expr> ] "]"
listOrListComp :: Parser Expr
listOrListComp = do
  openBracketToken
  choice
    [ closeBracketToken >> return (ListExpr [])
    , try listCompBody
    , normalListBody
    ]

listCompBody :: Parser Expr
listCompBody = do
  e <- logicExpr
  forToken
  varTok <- idToken
  inToken
  src <- expr
  cond <- optionMaybe (ifToken >> logicExpr)
  closeBracketToken
  updateState (symtable_insert_or_update (varTok, inTokenToVal inTok))
  return (ListCompExpr e (tokenName varTok) src cond)

inTokenToVal :: Token -> Token
inTokenToVal (Token p _) = Token p In

normalListBody :: Parser Expr
normalListBody = do
  elements <- sepBy logicExpr commaToken
  closeBracketToken
  return (ListExpr elements)

-- <dict> → "{" <kv_list> "}" | "{" "}"
dictParser :: Parser Expr
dictParser = do
  openBraceToken
  pairs <- sepBy kvPair commaToken
  closeBraceToken
  return (DictExpr pairs)

kvPair :: Parser (String, Expr)
kvPair = do
  kTok <- stringToken
  colonToken
  v <- logicExpr
  return (extractString kTok, v)

-- Execução

parser :: [Token] -> IO (Either ParseError (Program, SymbolTable))
parser tokens = runParserT (do { p <- program; st <- getState; return (p, st) }) [] "Parsing error!" tokens

runFile :: FilePath -> IO ()
runFile fn = do
  tokens <- getTokens fn
  result <- parser tokens
  case result of
    Left err -> print err
    Right (ast, symTable) -> do
      print ast
      print symTable

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Please inform the input filename. Closing application..."
    filenames -> mapM_ runFile filenames
