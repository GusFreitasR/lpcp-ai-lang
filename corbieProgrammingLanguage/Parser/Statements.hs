module Parser.Statements where

import Scanner
import Text.Parsec
import Parser.Types
import Parser.Tokens
import Parser.SymbolTable
import Parser.Expressions
import Parser.Semantic

blockStatements :: Parser [Stmt]
blockStatements = do
  openBraceToken
  statements <- many stmt
  closeBraceToken
  return statements

scopedBlock :: String -> Parser [Stmt]
scopedBlock prefix = do
  openBraceToken
  modifyState (enterAnonScope prefix)
  statements <- many stmt
  closeBraceToken
  modifyState exitScope
  return statements

stmt :: Parser Stmt
stmt = choice
  [ funcDefStmt, ifStmt, whileStmt, forStmt
  , assignOrCallStmt
  , printStmt <* semiColonToken
  , returnStmt <* semiColonToken
  , constDeclStmt <* semiColonToken
  , modelDefStmt, enumDefStmt
  ]

assignOrCallStmt :: Parser Stmt
assignOrCallStmt = do
  isCall <- optionMaybe (try (lookAhead (idToken >> openParenToken)))
  case isCall of
    Just _  -> funcCallStmt <* semiColonToken
    Nothing -> assignStmt <* semiColonToken

funcDefStmt :: Parser Stmt
funcDefStmt = do
  defTok <- defToken
  nameTok <- idToken
  openParenToken
  params <- sepBy idToken commaToken
  closeParenToken
  declareFunc nameTok defTok params
  stBefore <- getState
  let prevInFunc = inFunction stBefore
  modifyState (\s -> enterNamedScope (tokenName nameTok) (s { inFunction = True }))
  mapM_ (\param -> declareParam param defTok) params
  body <- blockStatements
  modifyState (\s -> (exitScope s) { inFunction = prevInFunc })
  return (FuncDefStmt (tokenName nameTok) (map tokenName params) body)

ifStmt :: Parser Stmt
ifStmt = do
  ifToken
  openParenToken
  condition <- logicExpr
  closeParenToken
  condType <- evalExpr condition
  checkBoolCondition "if" condType
  thenBlock <- scopedBlock "if"
  elseBlock <- optionMaybe elsePart
  return (IfStmt condition thenBlock elseBlock)

elsePart :: Parser [Stmt]
elsePart = do
  elseToken
  scopedBlock "else" <|> fmap (: []) ifStmt

whileStmt :: Parser Stmt
whileStmt = do
  whileToken
  openParenToken
  condition <- logicExpr
  closeParenToken
  condType <- evalExpr condition
  checkBoolCondition "while" condType
  body <- scopedBlock "while"
  return (WhileStmt condition body)

forStmt :: Parser Stmt
forStmt = do
  forToken
  openParenToken
  isForIn <- optionMaybe (try (lookAhead (idToken >> inToken)))
  case isForIn of
    Just _  -> forIn
    Nothing -> forTraditional

forTraditional :: Parser Stmt
forTraditional = do
  modifyState (enterAnonScope "for")
  initial <- assignParser
  semiColonToken
  condition <- logicExpr
  condType <- evalExpr condition
  checkBoolCondition "for" condType
  semiColonToken
  step <- assignParser
  closeParenToken
  body <- blockStatements
  modifyState exitScope
  return (ForStmt initial condition step body)

forIn :: Parser Stmt
forIn = do
  variable <- idToken
  inTok <- inToken
  source <- expr
  closeParenToken
  srcType <- evalExpr source
  if not (isIterable srcType)
    then semanticError ("'for ... in' source must be iterable, got " ++ show srcType)
    else do
      modifyState (enterAnonScope "for")
      declareSymbol variable inTok TDynamic Nothing
      body <- blockStatements
      modifyState exitScope
      return (ForInStmt (tokenName variable) source body)

assignStmt :: Parser Stmt
assignStmt = AssignStmt `fmap` assignParser

assignParser :: Parser Assign
assignParser = do
  (variable, baseTok) <- varParser
  operation <- assignOpParser
  value <- logicExpr
  checkAssign baseTok variable operation value
  return (AssignNode variable operation value)

assignOpParser :: Parser AssignOp
assignOpParser = choice
  [ assignToken >> return OpAssign, plusAssignToken >> return OpPlusAssign
  , minusAssignToken >> return OpMinusAssign, multAssignToken >> return OpMultAssign
  , divAssignToken >> return OpDivAssign
  ]

funcCallStmt :: Parser Stmt
funcCallStmt = do
  fc@(CallFunc fnName args) <- funcCallParser
  _ <- checkFuncCall fnName args
  return (FuncCallStmt fc)

returnStmt :: Parser Stmt
returnStmt = do
  returnToken
  mExpr <- optionMaybe logicExpr
  checkReturnStmt mExpr
  return (ReturnStmt mExpr)

printStmt :: Parser Stmt
printStmt = do
  printToken
  openParenToken
  value <- logicExpr
  closeParenToken
  _ <- evalExpr value
  return (PrintStmt value)

constDeclStmt :: Parser Stmt
constDeclStmt = do
  constToken
  nameTok <- idToken
  assignToken
  (value, mIdTok) <- constValParser
  declareConst nameTok value mIdTok
  return (ConstDeclStmt (tokenName nameTok) value)

constValParser :: Parser (Expr, Maybe Token)
constValParser = choice
  [ do t@(Token _ (IntLiteral n)) <- intToken; return (LitExpr (IntLit n), Just t)
  , do t@(Token _ (FloatLiteral n)) <- floatToken; return (LitExpr (FloatLit n), Just t)
  , do t@(Token _ (StringLiteral s)) <- stringToken; return (LitExpr (StringLit s), Just t)
  , do t@(Token _ (BoolLiteral b)) <- boolToken; return (LitExpr (BoolLit b), Just t)
  , do t <- idToken; return (VarExpr (SimpleVar (tokenName t)), Just t)
  ]

modelDefStmt :: Parser Stmt
modelDefStmt = do
  modelTok <- modelToken
  nameTok <- idToken
  declareModel nameTok modelTok
  modifyState (enterNamedScope (tokenName nameTok))
  body <- blockStatements
  modifyState exitScope
  return (ModelDefStmt (tokenName nameTok) body)

enumDefStmt :: Parser Stmt
enumDefStmt = do
  enumTok <- enumToken
  nameTok <- idToken
  openBraceToken
  members <- sepBy1 idToken commaToken
  closeBraceToken
  declareEnum nameTok enumTok members
  return (EnumDefStmt (tokenName nameTok) (map tokenName members))
