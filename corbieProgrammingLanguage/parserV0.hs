module Main (main) where

import Scanner
import Text.Parsec
import Text.Parsec.Pos

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  -- get_token (Token p Program) = Just (Token p Program)
  get_token token@(Token p Program) = Just token
  get_token _ = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token token@(Token p (Id x)) = Just token
  get_token _  = Nothing

varToken :: ParsecT [Token] st IO (Token)
varToken = tokenPrim show update_pos get_token where
  get_token token@(Token p Var) = Just token
  get_token _ = Nothing  

beginToken = tokenPrim show update_pos get_token where
  get_token token@(Token p Begin) = Just token
  get_token _ = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token token@(Token p End) = Just token
  get_token _ = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token token@(Token p SemiColon) = Just token
  get_token _ = Nothing

colonToken :: ParsecT [Token] st IO (Token)
colonToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Colon) = Just token
  get_token _ = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Assign) = Just token
  get_token _ = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Int x)) = Just token
  get_token _ = Nothing

typeToken :: ParsecT [Token] st IO (Token)
typeToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Type x)) = Just token
  get_token _ = Nothing 

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ [] = pos

-- parsers para os não-terminais
program :: ParsecT [Token] st IO ([Token])
program = do
            a <- programToken 
            b <- idToken 
            c <- beginToken 
            d <- stmts
            e <- endToken
            eof
            return (a:b:[c] ++ d ++ [e])

stmts :: ParsecT [Token] st IO ([Token])
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

assign :: ParsecT [Token] st IO ([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken
          return (a:b:[c])

remaining_stmts :: ParsecT [Token] st IO ([Token])
remaining_stmts = (do a <- semiColonToken
                      b <- assign
                      return (a:b)) <|> (return [])

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Parsing error!" tokens

main :: IO ()
main = do
    tokens <- getTokens "programaV0.pe"
    result <- parser tokens
    case result of
      { Left err -> print err; 
        Right ans -> print ans
      }            