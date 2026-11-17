{
module Scanner where

import System.IO
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$idstart = [$alpha \_]
$idchar = [$alpha $digit \_]
$stringchar = [^\"\\\n]
@identifier = $idstart $idchar*

tokens :-

  $white+                      ;
  "#".*                        ;

  "def"                        { \p _ -> Token p Def }
  "model"                      { \p _ -> Token p Model }
  "enum"                       { \p _ -> Token p Enum }
  "const"                      { \p _ -> Token p Const }
  "if"                         { \p _ -> Token p If }
  "else"                       { \p _ -> Token p Else }
  "for"                        { \p _ -> Token p For }
  "while"                      { \p _ -> Token p While }
  "return"                     { \p _ -> Token p Return }
  "print"                      { \p _ -> Token p Print }
  "and"                        { \p _ -> Token p And }
  "or"                         { \p _ -> Token p Or }
  "not"                        { \p _ -> Token p Not }
  "in"                         { \p _ -> Token p In }
  "cat"                        { \p _ -> Token p Cat }
  "true"                       { \p _ -> Token p (BoolLiteral True) }
  "false"                      { \p _ -> Token p (BoolLiteral False) }

  "{"                          { \p _ -> Token p OpenBrace }
  "}"                          { \p _ -> Token p CloseBrace }
  "("                          { \p _ -> Token p OpenParen }
  ")"                          { \p _ -> Token p CloseParen }
  "["                          { \p _ -> Token p OpenBracket }
  "]"                          { \p _ -> Token p CloseBracket }
  ","                          { \p _ -> Token p Comma }
  ";"                          { \p _ -> Token p SemiColon }
  ":"                          { \p _ -> Token p Colon }

  "<="                         { \p _ -> Token p LessEqual }
  ">="                         { \p _ -> Token p GreaterEqual }
  "=="                         { \p _ -> Token p Equal }
  "!="                         { \p _ -> Token p NotEqual }
  "+="                         { \p _ -> Token p PlusAssign }
  "-="                         { \p _ -> Token p MinusAssign }
  "*="                         { \p _ -> Token p MultAssign }
  "/="                         { \p _ -> Token p DivAssign }
  "**"                         { \p _ -> Token p Power }

  "="                          { \p _ -> Token p Assign }
  "+"                          { \p _ -> Token p Plus }
  "-"                          { \p _ -> Token p Minus }
  "*"                          { \p _ -> Token p Multiply }
  "/"                          { \p _ -> Token p Divide }
  "%"                          { \p _ -> Token p Modulo }
  "@"                          { \p _ -> Token p MatrixMult }
  "^"                          { \p _ -> Token p Caret }
  "<"                          { \p _ -> Token p Less }
  ">"                          { \p _ -> Token p Greater }

  $digit+ \. $digit+           { \p s -> Token p (FloatLiteral (read s)) }
  $digit+                      { \p s -> Token p (IntLiteral (read s)) }
  \" $stringchar* \"           { \p s -> Token p (StringLiteral (drop 1 (init s))) }
  @identifier                  { \p s -> Token p (Identifier s) }
  .                            { \p s -> Token p (LexicalError s) }

{

data Token =
    Token AlexPosn TokenClass
    deriving (Eq,Show)

data TokenClass =
  Def |
  Model |
  Enum |
  Const |
  If |
  Else |
  For |
  While |
  Return |
  Print |
  And |
  Or |
  Not |
  In |
  Cat |
  BoolLiteral Bool |
  OpenBrace |
  CloseBrace |
  OpenParen |
  CloseParen |
  OpenBracket |
  CloseBracket |
  Comma |
  SemiColon |
  Colon |
  Assign |
  PlusAssign |
  MinusAssign |
  MultAssign |
  DivAssign |
  Power |
  Plus |
  Minus |
  Multiply |
  Divide |
  Modulo |
  MatrixMult |
  Caret |
  Less |
  LessEqual |
  Greater |
  GreaterEqual |
  Equal |
  NotEqual |
  IntLiteral Int |
  FloatLiteral Double |
  StringLiteral String |
  Identifier String |
  LexicalError String
  deriving (Eq,Show)

getTokens :: FilePath -> IO [Token]
getTokens fn = do
  s <- readFile fn
  return (alexScanTokens s)
}