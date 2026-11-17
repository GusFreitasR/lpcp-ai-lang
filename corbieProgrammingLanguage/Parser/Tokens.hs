module Parser.Tokens where

import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Parser.Types

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ [] = pos

matchToken :: (TokenClass -> Bool) -> Parser Token
matchToken predicate = tokenPrim show update_pos getToken
  where
    getToken token@(Token _ tokenClass)
      | predicate tokenClass = Just token
      | otherwise = Nothing

tokenName :: Token -> String
tokenName (Token _ (Identifier name)) = name
tokenName (Token _ tokenClass) = show tokenClass

tokenParser :: (TokenClass -> Bool) -> Parser Token
tokenParser = matchToken

defToken = tokenParser (\tc -> case tc of Def -> True; _ -> False)
modelToken = tokenParser (\tc -> case tc of Model -> True; Identifier "model" -> True; _ -> False)
enumToken = tokenParser (\tc -> case tc of Enum -> True; Identifier "enum" -> True; _ -> False)
constToken = tokenParser (\tc -> case tc of Const -> True; Identifier "const" -> True; _ -> False)
ifToken = tokenParser (\tc -> case tc of If -> True; _ -> False)
elseToken = tokenParser (\tc -> case tc of Else -> True; _ -> False)
forToken = tokenParser (\tc -> case tc of For -> True; _ -> False)
whileToken = tokenParser (\tc -> case tc of While -> True; _ -> False)
returnToken = tokenParser (\tc -> case tc of Return -> True; _ -> False)
printToken = tokenParser (\tc -> case tc of Print -> True; Identifier "print" -> True; _ -> False)
andToken = tokenParser (\tc -> case tc of And -> True; _ -> False)
orToken = tokenParser (\tc -> case tc of Or -> True; _ -> False)
notToken = tokenParser (\tc -> case tc of Not -> True; Identifier "not" -> True; _ -> False)
inToken = tokenParser (\tc -> case tc of In -> True; Identifier "in" -> True; _ -> False)
catToken = tokenParser (\tc -> case tc of Cat -> True; Identifier "cat" -> True; _ -> False)

openBraceToken = tokenParser (\tc -> case tc of OpenBrace -> True; _ -> False)
closeBraceToken = tokenParser (\tc -> case tc of CloseBrace -> True; _ -> False)
openParenToken = tokenParser (\tc -> case tc of OpenParen -> True; _ -> False)
closeParenToken = tokenParser (\tc -> case tc of CloseParen -> True; _ -> False)
openBracketToken = tokenParser (\tc -> case tc of OpenBracket -> True; _ -> False)
closeBracketToken = tokenParser (\tc -> case tc of CloseBracket -> True; _ -> False)
commaToken = tokenParser (\tc -> case tc of Comma -> True; _ -> False)
semiColonToken = tokenParser (\tc -> case tc of SemiColon -> True; _ -> False)
colonToken = tokenParser (\tc -> case tc of Colon -> True; _ -> False)

assignToken = tokenParser (\tc -> case tc of Assign -> True; _ -> False)
plusAssignToken = tokenParser (\tc -> case tc of PlusAssign -> True; _ -> False)
minusAssignToken = tokenParser (\tc -> case tc of MinusAssign -> True; _ -> False)
multAssignToken = tokenParser (\tc -> case tc of MultAssign -> True; _ -> False)
divAssignToken = tokenParser (\tc -> case tc of DivAssign -> True; _ -> False)
plusToken = tokenParser (\tc -> case tc of Plus -> True; _ -> False)
minusToken = tokenParser (\tc -> case tc of Minus -> True; _ -> False)
multiplyToken = tokenParser (\tc -> case tc of Multiply -> True; _ -> False)
divideToken = tokenParser (\tc -> case tc of Divide -> True; _ -> False)
moduloToken = tokenParser (\tc -> case tc of Modulo -> True; _ -> False)
matrixMultToken = tokenParser (\tc -> case tc of MatrixMult -> True; _ -> False)
powerToken = tokenParser (\tc -> case tc of Power -> True; _ -> False)
caretToken = tokenParser (\tc -> case tc of Caret -> True; _ -> False)
equalToken = tokenParser (\tc -> case tc of Equal -> True; _ -> False)
notEqualToken = tokenParser (\tc -> case tc of NotEqual -> True; _ -> False)
lessEqualToken = tokenParser (\tc -> case tc of LessEqual -> True; _ -> False)
greaterEqualToken = tokenParser (\tc -> case tc of GreaterEqual -> True; _ -> False)
lessToken = tokenParser (\tc -> case tc of Less -> True; _ -> False)
greaterToken = tokenParser (\tc -> case tc of Greater -> True; _ -> False)

literalToken :: (TokenClass -> Bool) -> Parser Token
literalToken = tokenParser

intToken = literalToken (\tc -> case tc of IntLiteral _ -> True; _ -> False)
floatToken = literalToken (\tc -> case tc of FloatLiteral _ -> True; _ -> False)
stringToken = literalToken (\tc -> case tc of StringLiteral _ -> True; _ -> False)
boolToken = literalToken (\tc -> case tc of BoolLiteral _ -> True; _ -> False)

idToken = tokenParser isIdentifier
  where
    isIdentifier (Identifier name) = name `notElem`
      ["def", "model", "enum", "const", "if", "else", "for", "while", "return",
       "print", "and", "or", "not", "in", "cat", "true", "false"]
    isIdentifier _ = False
