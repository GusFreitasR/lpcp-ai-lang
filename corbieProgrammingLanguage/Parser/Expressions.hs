module Parser.Expressions where

import Scanner
import Text.Parsec
import Parser.Types
import Parser.Tokens

logicExpr :: Parser Expr
logicExpr = chainl1 logicTerm (orToken >> return (BinaryExpr OpOr))

logicTerm :: Parser Expr
logicTerm = chainl1 relExpr (andToken >> return (BinaryExpr OpAnd))

relExpr :: Parser Expr
relExpr = choice
  [ do notToken; UnaryExpr OpNot `fmap` relExpr
  , try (do left <- expr; op <- relOp; right <- expr; return (BinaryExpr op left right))
  , expr
  ]

relOp :: Parser BinOp
relOp = choice
  [ equalToken >> return OpEqual, notEqualToken >> return OpNotEqual
  , lessEqualToken >> return OpLessEqual, greaterEqualToken >> return OpGreaterEqual
  , lessToken >> return OpLess, greaterToken >> return OpGreater
  , inToken >> return OpIn
  ]

expr :: Parser Expr
expr = chainl1 term (choice
  [ plusToken >> return (BinaryExpr OpAdd)
  , minusToken >> return (BinaryExpr OpSub)
  , catToken >> return (BinaryExpr OpCat)
  ])

term :: Parser Expr
term = chainl1 factor (choice
  [ multiplyToken >> return (BinaryExpr OpMul)
  , divideToken >> return (BinaryExpr OpDiv)
  , moduloToken >> return (BinaryExpr OpMod)
  , matrixMultToken >> return (BinaryExpr OpMatMul)
  ])

factor :: Parser Expr
factor = do
  value <- unary
  choice
    [ do powerToken; BinaryExpr OpPow value `fmap` factor
    , do caretToken; BinaryExpr OpPow value `fmap` factor
    , return value
    ]

unary :: Parser Expr
unary = choice
  [ minusToken >> UnaryExpr OpNeg `fmap` unary
  , plusToken >> UnaryExpr OpPos `fmap` unary
  , primary
  ]

primary :: Parser Expr
primary = choice
  [ (LitExpr . IntLit . extractInt) `fmap` intToken
  , (LitExpr . FloatLit . extractFloat) `fmap` floatToken
  , (LitExpr . StringLit . extractString) `fmap` stringToken
  , (LitExpr . BoolLit . extractBool) `fmap` boolToken
  , between openParenToken closeParenToken logicExpr
  , listOrListComp
  , dictParser
  , try (FuncCallExpr `fmap` funcCallParser)
  , (VarExpr . fst) `fmap` varParser
  ]

extractInt :: Token -> Int
extractInt (Token _ (IntLiteral n)) = n
extractInt _ = 0

extractFloat :: Token -> Double
extractFloat (Token _ (FloatLiteral n)) = n
extractFloat _ = 0.0

extractString :: Token -> String
extractString (Token _ (StringLiteral value)) = value
extractString _ = ""

extractBool :: Token -> Bool
extractBool (Token _ (BoolLiteral value)) = value
extractBool _ = False

funcCallParser :: Parser FuncCall
funcCallParser = do
  nameTok <- idToken
  openParenToken
  args <- sepBy logicExpr commaToken
  closeParenToken
  return (CallFunc (tokenName nameTok) args)

varParser :: Parser (Var, Token)
varParser = do
  baseTok <- idToken
  suffixes <- many varIndexSuffix
  return (foldl IndexVar (SimpleVar (tokenName baseTok)) suffixes, baseTok)

varIndexSuffix :: Parser [Slice]
varIndexSuffix = do
  openBracketToken
  slices <- sepBy1 sliceParser commaToken
  closeBracketToken
  return slices

sliceParser :: Parser Slice
sliceParser = choice [try sliceBoth, try sliceFrom, try sliceTo, try sliceAll, sliceIndex]

sliceBoth :: Parser Slice
sliceBoth = do a <- expr; colonToken; b <- expr; return (SliceFromTo (Just a) (Just b))

sliceFrom :: Parser Slice
sliceFrom = do a <- expr; colonToken; return (SliceFromTo (Just a) Nothing)

sliceTo :: Parser Slice
sliceTo = do colonToken; b <- expr; return (SliceFromTo Nothing (Just b))

sliceAll :: Parser Slice
sliceAll = colonToken >> return (SliceFromTo Nothing Nothing)

sliceIndex :: Parser Slice
sliceIndex = SliceSingle `fmap` expr

listOrListComp :: Parser Expr
listOrListComp = do
  openBracketToken
  choice [closeBracketToken >> return (ListExpr []), try listCompBody, normalListBody]

listCompBody :: Parser Expr
listCompBody = do
  value <- logicExpr
  forToken
  varTok <- idToken
  _ <- inToken
  source <- expr
  condition <- optionMaybe (ifToken >> logicExpr)
  closeBracketToken
  return (ListCompExpr value (tokenName varTok) source condition)

normalListBody :: Parser Expr
normalListBody = do
  elements <- sepBy logicExpr commaToken
  closeBracketToken
  return (ListExpr elements)

dictParser :: Parser Expr
dictParser = do
  openBraceToken
  pairs <- sepBy kvPair commaToken
  closeBraceToken
  return (DictExpr pairs)

kvPair :: Parser (String, Expr)
kvPair = do
  key <- stringToken
  colonToken
  value <- logicExpr
  return (extractString key, value)
