module Parser.Pretty (prettyProgram, prettySymbolTable) where

import Scanner (Token(..), TokenClass(..))
import Parser.Types

data Tree = Tree String [Tree]

prettyProgram :: Program -> String
prettyProgram (Program statements) =
  renderTree (Tree "Program" (map prettyStmt statements))

prettyStmt :: Stmt -> Tree
prettyStmt stmt = case stmt of
  FuncDefStmt name params body ->
    Tree ("FuncDefStmt " ++ show name ++ " " ++ show params) (map prettyStmt body)
  ModelDefStmt name body ->
    Tree ("ModelDefStmt " ++ show name) (map prettyStmt body)
  EnumDefStmt name members ->
    Tree ("EnumDefStmt " ++ show name ++ " " ++ show members) []
  ConstDeclStmt name expr ->
    Tree ("ConstDeclStmt " ++ show name) [prettyExpr expr]
  AssignStmt assignment ->
    Tree "AssignStmt" [prettyAssign assignment]
  FuncCallStmt call ->
    Tree "FuncCallStmt" [prettyCall call]
  IfStmt condition thenBody elseBody ->
    Tree "IfStmt"
      ([Tree "condition" [prettyExpr condition], Tree "then" (map prettyStmt thenBody)]
        ++ maybe [] (\body -> [Tree "else" (map prettyStmt body)]) elseBody)
  WhileStmt condition body ->
    Tree "WhileStmt" [Tree "condition" [prettyExpr condition], Tree "body" (map prettyStmt body)]
  ForStmt start condition step body ->
    Tree "ForStmt"
      [ prettyAssign start
      , Tree "condition" [prettyExpr condition]
      , prettyAssign step
      , Tree "body" (map prettyStmt body)
      ]
  ForInStmt name collection body ->
    Tree ("ForInStmt " ++ show name)
      [Tree "collection" [prettyExpr collection], Tree "body" (map prettyStmt body)]
  ReturnStmt maybeExpr ->
    Tree "ReturnStmt" (maybe [] (\expr -> [prettyExpr expr]) maybeExpr)
  PrintStmt expr ->
    Tree "PrintStmt" [prettyExpr expr]

prettyAssign :: Assign -> Tree
prettyAssign (AssignNode var op expr) =
  Tree "AssignNode" [prettyVar var, Tree (show op) [], prettyExpr expr]

prettyVar :: Var -> Tree
prettyVar var = case var of
  SimpleVar name -> Tree ("SimpleVar " ++ show name) []
  IndexVar base slices -> Tree "IndexVar" (prettyVar base : map prettySlice slices)

prettySlice :: Slice -> Tree
prettySlice slice = case slice of
  SliceSingle expr -> Tree "SliceSingle" [prettyExpr expr]
  SliceFromTo from to ->
    Tree "SliceFromTo"
      (maybe [] (\expr -> [Tree "from" [prettyExpr expr]]) from
        ++ maybe [] (\expr -> [Tree "to" [prettyExpr expr]]) to)

prettyCall :: FuncCall -> Tree
prettyCall (CallFunc name args) =
  Tree ("CallFunc " ++ show name) (map prettyExpr args)

prettyExpr :: Expr -> Tree
prettyExpr expr = case expr of
  LitExpr literal -> Tree ("LitExpr (" ++ show literal ++ ")") []
  VarExpr var -> Tree "VarExpr" [prettyVar var]
  FuncCallExpr call -> Tree "FuncCallExpr" [prettyCall call]
  ListExpr elements -> Tree "ListExpr" (map prettyExpr elements)
  ListCompExpr element name collection condition ->
    Tree ("ListCompExpr " ++ show name)
      ([Tree "element" [prettyExpr element], Tree "collection" [prettyExpr collection]]
        ++ maybe [] (\expr -> [Tree "condition" [prettyExpr expr]]) condition)
  DictExpr entries ->
    Tree "DictExpr" [Tree ("Entry " ++ show key) [prettyExpr value] | (key, value) <- entries]
  BinaryExpr op left right ->
    Tree ("BinaryExpr " ++ show op) [prettyExpr left, prettyExpr right]
  UnaryExpr op value ->
    Tree ("UnaryExpr " ++ show op) [prettyExpr value]

renderTree :: Tree -> String
renderTree (Tree label children) =
  unlines (label : renderChildren "" children)

renderChildren :: String -> [Tree] -> [String]
renderChildren _ [] = []
renderChildren prefix [Tree label children] =
  (prefix ++ "`-- " ++ label)
    : renderChildren (prefix ++ "    ") children
renderChildren prefix (Tree label children : rest) =
  (prefix ++ "|-- " ++ label)
    : renderChildren (prefix ++ "|   ") children
    ++ renderChildren prefix rest

prettySymbolTable :: SymbolTable -> String
prettySymbolTable [] = "Symbol table: (empty)"
prettySymbolTable symbols =
  "Symbol table:\n" ++ unlines (map prettySymbol symbols)
  where
    prettySymbol (Token _ (Identifier name), Token _ value) =
      "  " ++ name ++ " = " ++ prettyTokenClass value
    prettySymbol (Token _ name, Token _ value) =
      "  " ++ prettyTokenClass name ++ " = " ++ prettyTokenClass value

prettyTokenClass :: TokenClass -> String
prettyTokenClass tokenClass = case tokenClass of
  Identifier name -> name
  IntLiteral value -> show value
  FloatLiteral value -> show value
  StringLiteral value -> show value
  BoolLiteral value -> show value
  _ -> show tokenClass
