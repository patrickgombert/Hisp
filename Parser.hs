module Parser where

import Runtime
import Text.Show.Functions
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

parseNum :: Parser Expression
parseNum = do intPart <- many1 digit
              result <- optionMaybe (char '.')
              case result of
                Just _  -> do fractionPart <- many1 digit
                              return $ Efloat (read $ intPart ++ "." ++ fractionPart)
                Nothing -> return $ Eint (read intPart)

lexer = makeTokenParser emptyDef { reservedNames = ["true", "false"] }
hispReserved = reserved lexer

parseBoolT :: Parser Expression
parseBoolT = do hispReserved "true"
                return (Ebool True)

parseBoolF :: Parser Expression
parseBoolF = do hispReserved "false"
                return (Ebool False)

parseSym :: Parser Expression
parseSym = do symName <- many1 (letter <|> char '-' <|> char '=' <|> char '!' <|> char '+' <|> char '*' <|> char '/')
              result <- optionMaybe (char '/')
              case result of
                Just _ -> do symNamePart2 <- many1 (letter <|> char '-' <|> char '=' <|> char '!' <|> char '+' <|> char '*' <|> char '/')
                             return $ Esym (Just (Vstr symName)) (Vstr symNamePart2)
                Nothing -> return $ Esym Nothing (Vstr symName)

parseStr :: Parser Expression
parseStr = do char '"'
              str <- many $ noneOf ['"']
              char '"'
              return (Estr str)

parseList :: Parser Expression
parseList = do char '('
               exprs <- many parseExpr
               char ')'
               return (Elist exprs)

parseExpr :: Parser Expression
parseExpr = do spaces
               result <- try parseNum <|> parseBoolT <|> parseBoolF <|> parseSym <|> parseStr <|> parseList
               spaces
               return result

lispParser :: Parser [Expression]
lispParser = do results <- many1 parseExpr
                spaces
                eof
                return results

parseProgram :: String -> [Expression]
parseProgram program = case parse lispParser "" program of
                         Left err -> error $ (show err)
                         Right exprs -> exprs

