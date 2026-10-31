{
module Scanner where

import System.IO -- manipulação de arquivos
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
  "if"                         { \p _ -> Token p If }
  "else"                       { \p _ -> Token p Else }
  "for"                        { \p _ -> Token p For }
  "while"                      { \p _ -> Token p While }
  "return"                     { \p _ -> Token p Return }
  "and"                        { \p _ -> Token p And }
  "or"                         { \p _ -> Token p Or }

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
  "++"                         { \p _ -> Token p Increment }

  "="                          { \p _ -> Token p Assign }
  "+"                          { \p _ -> Token p Plus }
  "-"                          { \p _ -> Token p Minus }
  "*"                          { \p _ -> Token p Multiply }
  "/"                          { \p _ -> Token p Divide }
  "<"                          { \p _ -> Token p Less }
  ">"                          { \p _ -> Token p Greater }

  $digit+                      { \p s -> Token p (IntLiteral (read s)) }
  \" $stringchar* \"           { \p s -> Token p (StringLiteral (drop 1 (init s))) }
  @identifier                  { \p s -> Token p (Identifier s) }
  .                            { \p s -> Token p (LexicalError s) }

{

data Token =
    Token AlexPosn TokenClass
    deriving (Eq,Show)

-- The token class type:
data TokenClass =
  Def |
  If |
  Else |
  For |
  While |
  Return |
  And |
  Or |
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
  Increment |
  Plus |
  Minus |
  Multiply |
  Divide |
  Less |
  LessEqual |
  Greater |
  GreaterEqual |
  Equal |
  NotEqual |
  IntLiteral Int |
  StringLiteral String |
  Identifier String |
  LexicalError String
  deriving (Eq,Show)

getTokens :: FilePath -> IO [Token]
getTokens fn = do
  s <- readFile fn
  return (alexScanTokens s)
}