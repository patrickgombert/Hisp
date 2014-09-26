module Parser where

import Runtime
import Text.Show.Functions
import Text.ParserCombinators.Parsec hiding (State)

parseSym :: Parser Expression
parseSym = do symName <- many1 (letter <|> char '-')
              result <- optionMaybe (char '/')
              case result of
                Just _ -> do symNamePart2 <- many1 (letter <|> char '-')
                             return $ Esym (Just (Vstr symName)) (Vstr symNamePart2)
                Nothing -> return $ Esym Nothing (Vstr symName)

parseNum :: Parser Expression
parseNum = do intPart <- many1 digit
              result <- optionMaybe (char '.')
              case result of
                Just _  -> do fractionPart <- many1 digit
                              return $ Efloat (read $ intPart ++ "." ++ fractionPart)
                Nothing -> return $ Eint (read intPart)

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
               result <- parseSym <|> parseNum <|> parseStr <|> parseList
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

