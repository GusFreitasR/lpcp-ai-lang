{
module Scanner where

import System.IO -- manipulação de arquivos
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                                ;
  "--".*                                 ;
  program                                { \p s -> Token p Program}
  var                                    { \p s -> Token p Var}
  begin                                  { \p s -> Token p Begin}
  end                                    { \p s -> Token p End}
  :                                      { \p s -> Token p Colon}
  ";"                                    { \p s -> Token p SemiColon}
  int                                    { \p s -> Token p (Type s)}
  :=                                     { \p s -> Token p Assign}
  if                                     { \p s -> Token p If}
  then                                   { \p s -> Token p Then}
  write                                  { \p s -> Token p Write}
  >                                      { \p s -> Token p Greater}
  $digit+                                { \p s -> Token p (Int (read s))}
  $alpha [$alpha $digit \_ \']*          { \p s -> Token p (Id s)}
  \" $alpha [$alpha $digit ! \_ \']* \"  { \p s -> Token p (String s)}

{

-- Each action has type :: String -> Token

-- The token type:
data Token =
    Token AlexPosn TokenClass
    deriving (Eq,Show)

-- The token class type:
data TokenClass =
  Program |
  Var |
  Begin |
  End |
  Colon |
  SemiColon |
  Assign | 
  If |
  Then |
  Write |
  Greater |
  Type String |
  Id String |
  Int Int |
  String String
  deriving (Eq,Show)

getTokens fn = do
                  fh <- openFile fn ReadMode;
                  s <- hGetContents fh;
                  return (alexScanTokens s)
}