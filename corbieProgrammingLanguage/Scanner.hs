module Scanner (
    AlexPosn(..),
    Token(..),
    TokenClass(..),
    alexScanTokens,
    getTokens
) where

import System.IO
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

data AlexPosn = AlexPn !Int !Int !Int
    deriving (Eq, Show)

data Token =
    Token AlexPosn TokenClass
    deriving (Eq, Show)

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
    deriving (Eq, Show)

getTokens :: FilePath -> IO [Token]
getTokens fn = do
    s <- readFile fn
    return (alexScanTokens s)

alexScanTokens :: String -> [Token]
alexScanTokens = scan 0 1 1
  where
    scan :: Int -> Int -> Int -> String -> [Token]
    scan _ _ _ [] = []

    -- Newlines
    scan offset line col ('\r':'\n':cs) = scan (offset + 2) (line + 1) 1 cs
    scan offset line col ('\n':cs)       = scan (offset + 1) (line + 1) 1 cs

    -- Whitespace
    scan offset line col (c:cs)
      | isSpace c = scan (offset + 1) line (col + 1) cs

    -- Comments (# to end of line)
    scan offset line col ('#':cs) =
      let (_, rest) = break (\ch -> ch == '\n' || ch == '\r') cs
      in scan (offset + 1 + length cs - length rest) line col rest

    -- Two-character operators
    scan offset line col ('<':'=':cs) = Token (AlexPn offset line col) LessEqual : scan (offset + 2) line (col + 2) cs
    scan offset line col ('>':'=':cs) = Token (AlexPn offset line col) GreaterEqual : scan (offset + 2) line (col + 2) cs
    scan offset line col ('=':'=':cs) = Token (AlexPn offset line col) Equal : scan (offset + 2) line (col + 2) cs
    scan offset line col ('!':'=':cs) = Token (AlexPn offset line col) NotEqual : scan (offset + 2) line (col + 2) cs
    scan offset line col ('+':'=':cs) = Token (AlexPn offset line col) PlusAssign : scan (offset + 2) line (col + 2) cs
    scan offset line col ('-':'=':cs) = Token (AlexPn offset line col) MinusAssign : scan (offset + 2) line (col + 2) cs
    scan offset line col ('*':'=':cs) = Token (AlexPn offset line col) MultAssign : scan (offset + 2) line (col + 2) cs
    scan offset line col ('/':'=':cs) = Token (AlexPn offset line col) DivAssign : scan (offset + 2) line (col + 2) cs
    scan offset line col ('*':'*':cs) = Token (AlexPn offset line col) Power : scan (offset + 2) line (col + 2) cs

    -- Single-character delimiters and operators
    scan offset line col ('{':cs) = Token (AlexPn offset line col) OpenBrace : scan (offset + 1) line (col + 1) cs
    scan offset line col ('}':cs) = Token (AlexPn offset line col) CloseBrace : scan (offset + 1) line (col + 1) cs
    scan offset line col ('(':cs) = Token (AlexPn offset line col) OpenParen : scan (offset + 1) line (col + 1) cs
    scan offset line col (')':cs) = Token (AlexPn offset line col) CloseParen : scan (offset + 1) line (col + 1) cs
    scan offset line col ('[':cs) = Token (AlexPn offset line col) OpenBracket : scan (offset + 1) line (col + 1) cs
    scan offset line col (']':cs) = Token (AlexPn offset line col) CloseBracket : scan (offset + 1) line (col + 1) cs
    scan offset line col (',':cs) = Token (AlexPn offset line col) Comma : scan (offset + 1) line (col + 1) cs
    scan offset line col (';':cs) = Token (AlexPn offset line col) SemiColon : scan (offset + 1) line (col + 1) cs
    scan offset line col (':':cs) = Token (AlexPn offset line col) Colon : scan (offset + 1) line (col + 1) cs
    scan offset line col ('=':cs) = Token (AlexPn offset line col) Assign : scan (offset + 1) line (col + 1) cs
    scan offset line col ('+':cs) = Token (AlexPn offset line col) Plus : scan (offset + 1) line (col + 1) cs
    scan offset line col ('-':cs) = Token (AlexPn offset line col) Minus : scan (offset + 1) line (col + 1) cs
    scan offset line col ('*':cs) = Token (AlexPn offset line col) Multiply : scan (offset + 1) line (col + 1) cs
    scan offset line col ('/':cs) = Token (AlexPn offset line col) Divide : scan (offset + 1) line (col + 1) cs
    scan offset line col ('%':cs) = Token (AlexPn offset line col) Modulo : scan (offset + 1) line (col + 1) cs
    scan offset line col ('@':cs) = Token (AlexPn offset line col) MatrixMult : scan (offset + 1) line (col + 1) cs
    scan offset line col ('^':cs) = Token (AlexPn offset line col) Caret : scan (offset + 1) line (col + 1) cs
    scan offset line col ('<':cs) = Token (AlexPn offset line col) Less : scan (offset + 1) line (col + 1) cs
    scan offset line col ('>':cs) = Token (AlexPn offset line col) Greater : scan (offset + 1) line (col + 1) cs

    -- String literals
    scan offset line col ('"':cs) =
      let (strChars, rest) = break (\ch -> ch == '"' || ch == '\n' || ch == '\r') cs
      in case rest of
           ('"':after) ->
             let len = length strChars + 2
             in Token (AlexPn offset line col) (StringLiteral strChars) : scan (offset + len) line (col + len) after
           _ ->
             Token (AlexPn offset line col) (LexicalError ("Unterminated string: " ++ strChars)) : []

    -- Number literals (float or int)
    scan offset line col input@(c:_)
      | isDigit c =
          let (digits, rest1) = span isDigit input
          in case rest1 of
               ('.':d:rest2) | isDigit d ->
                 let (moreDigits, rest3) = span isDigit (d:rest2)
                     numStr = digits ++ "." ++ moreDigits
                     len = length numStr
                 in Token (AlexPn offset line col) (FloatLiteral (read numStr)) : scan (offset + len) line (col + len) rest3
               _ ->
                 let len = length digits
                 in Token (AlexPn offset line col) (IntLiteral (read digits)) : scan (offset + len) line (col + len) rest1

    -- Identifiers and keywords
    scan offset line col input@(c:_)
      | isAlpha c || c == '_' =
          let (ident, rest) = span (\ch -> isAlphaNum ch || ch == '_') input
              len = length ident
              pos = AlexPn offset line col
              tokClass = case ident of
                "def"    -> Def
                "model"  -> Model
                "enum"   -> Enum
                "const"  -> Const
                "if"     -> If
                "else"   -> Else
                "for"    -> For
                "while"  -> While
                "return" -> Return
                "print"  -> Print
                "and"    -> And
                "or"     -> Or
                "not"    -> Not
                "in"     -> In
                "cat"    -> Cat
                "true"   -> BoolLiteral True
                "false"  -> BoolLiteral False
                _        -> Identifier ident
          in Token pos tokClass : scan (offset + len) line (col + len) rest

    -- Lexical error for any unhandled character
    scan offset line col (c:cs) =
      Token (AlexPn offset line col) (LexicalError [c]) : scan (offset + 1) line (col + 1) cs
