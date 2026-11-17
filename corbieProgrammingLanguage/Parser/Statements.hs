module Parser.Statements where

import Scanner
import Text.Parsec
import Parser.Types
import Parser.Tokens
import Parser.SymbolTable
import Parser.Expressions

block :: Parser [Stmt]
block = do
  openBraceToken
  updateState enterScope
  statements <- many stmt
  closeBraceToken
  updateState exitScope
  return statements

stmt :: Parser Stmt
stmt = choice
  [ funcDefStmt, ifStmt, whileStmt, forStmt
  , try (assignStmt <* semiColonToken)
  , try (funcCallStmt <* semiColonToken)
  , printStmt <* semiColonToken
  , returnStmt <* semiColonToken
  , try (constDeclStmt <* semiColonToken)
  , modelDefStmt, enumDefStmt
  ]

funcDefStmt :: Parser Stmt
funcDefStmt = do
  defTok <- defToken
  nameTok <- idToken
  openParenToken
  params <- sepBy idToken commaToken
  closeParenToken
  updateState (symtable_insert (nameTok, defTok))
  updateState enterScope
  mapM_ (\param -> updateState (symtable_insert (param, defTok))) params
  body <- block
  updateState exitScope
  return (FuncDefStmt (tokenName nameTok) (map tokenName params) body)

ifStmt :: Parser Stmt
ifStmt = do
  ifToken
  openParenToken
  condition <- logicExpr
  closeParenToken
  thenBlock <- block
  elseBlock <- optionMaybe elsePart
  return (IfStmt condition thenBlock elseBlock)

elsePart :: Parser [Stmt]
elsePart = do
  elseToken
  block <|> fmap (: []) ifStmt

whileStmt :: Parser Stmt
whileStmt = do
  whileToken
  openParenToken
  condition <- logicExpr
  closeParenToken
  body <- block
  return (WhileStmt condition body)

forStmt :: Parser Stmt
forStmt = do
  forToken
  openParenToken
  try forTraditional <|> forIn

forTraditional :: Parser Stmt
forTraditional = do
  updateState enterScope
  initial <- assignParser
  semiColonToken
  condition <- logicExpr
  semiColonToken
  step <- assignParser
  closeParenToken
  body <- block
  updateState exitScope
  return (ForStmt initial condition step body)

forIn :: Parser Stmt
forIn = do
  variable <- idToken
  inTok <- inToken
  source <- expr
  closeParenToken
  updateState enterScope
  updateState (symtable_insert (variable, inTok))
  body <- block
  updateState exitScope
  return (ForInStmt (tokenName variable) source body)

assignStmt :: Parser Stmt
assignStmt = AssignStmt `fmap` assignParser

assignParser :: Parser Assign
assignParser = do
  (variable, baseTok) <- varParser
  operation <- assignOpParser
  value <- logicExpr
  let tokenValue = case value of
        LitExpr (IntLit n) -> Token (alexPos baseTok) (IntLiteral n)
        LitExpr (FloatLit n) -> Token (alexPos baseTok) (FloatLiteral n)
        LitExpr (StringLit s) -> Token (alexPos baseTok) (StringLiteral s)
        LitExpr (BoolLit b) -> Token (alexPos baseTok) (BoolLiteral b)
        _ -> Token (alexPos baseTok) Assign
  symbols <- getState
  case operation of
    OpAssign -> case symtable_lookup baseTok symbols of
      Just _ -> updateState (symtable_update (baseTok, tokenValue))
      Nothing -> updateState (symtable_insert (baseTok, tokenValue))
    _ -> updateState (symtable_update (baseTok, tokenValue))
  return (AssignNode variable operation value)

alexPos (Token position _) = position

assignOpParser :: Parser AssignOp
assignOpParser = choice
  [ assignToken >> return OpAssign, plusAssignToken >> return OpPlusAssign
  , minusAssignToken >> return OpMinusAssign, multAssignToken >> return OpMultAssign
  , divAssignToken >> return OpDivAssign
  ]

funcCallStmt :: Parser Stmt
funcCallStmt = FuncCallStmt `fmap` funcCallParser

returnStmt :: Parser Stmt
returnStmt = returnToken >> fmap ReturnStmt (optionMaybe logicExpr)

printStmt :: Parser Stmt
printStmt = do
  printToken
  openParenToken
  value <- logicExpr
  closeParenToken
  return (PrintStmt value)

constDeclStmt :: Parser Stmt
constDeclStmt = do
  constToken
  nameTok <- idToken
  assignToken
  (value, valueTok) <- constValParser
  updateState (symtable_insert (nameTok, valueTok))
  return (ConstDeclStmt (tokenName nameTok) value)

constValParser :: Parser (Expr, Token)
constValParser = choice
  [ do t@(Token _ (IntLiteral n)) <- intToken; return (LitExpr (IntLit n), t)
  , do t@(Token _ (FloatLiteral n)) <- floatToken; return (LitExpr (FloatLit n), t)
  , do t@(Token _ (StringLiteral s)) <- stringToken; return (LitExpr (StringLit s), t)
  , do t@(Token _ (BoolLiteral b)) <- boolToken; return (LitExpr (BoolLit b), t)
  , do t <- idToken; return (VarExpr (SimpleVar (tokenName t)), t)
  ]

modelDefStmt :: Parser Stmt
modelDefStmt = do
  modelTok <- modelToken
  nameTok <- idToken
  body <- block
  updateState (symtable_insert (nameTok, modelTok))
  return (ModelDefStmt (tokenName nameTok) body)

enumDefStmt :: Parser Stmt
enumDefStmt = do
  enumTok <- enumToken
  nameTok <- idToken
  openBraceToken
  members <- sepBy1 idToken commaToken
  closeBraceToken
  updateState (symtable_insert (nameTok, enumTok))
  mapM_ (\member -> updateState (symtable_insert (member, enumTok))) members
  return (EnumDefStmt (tokenName nameTok) (map tokenName members))
