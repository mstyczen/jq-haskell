module Jq.JParser where

import Parsing.Parsing
import Jq.Json

-- define parsing for each
parseJNull :: Parser JSON
parseJNull = do 
    _ <- string "null"
    return JNull

-- parseJString :: Parser JSON
parseJString :: Parser JSON
parseJString = do
    s <- parseString
    return (JString s)

parseString :: Parser String 
parseString = do
    _ <- symbol "\""
    s <- many (sat (/= '"'))
    _ <- symbol "\""
    return s

parseJBoolean :: Parser JSON 
parseJBoolean = do 
    x <- string "false" <|> string "true"
    if x == "false" then return (JBoolean False) else return (JBoolean True)

parseJNumber :: Parser JSON 
parseJNumber = parseJScientific <|> parseJStandard <|> parseJInt

parseJInt :: Parser JSON 
parseJInt = do
    x <- int
    return (JNumber (fromIntegral x))

parseJStandard :: Parser JSON 
parseJStandard = do
    l <- int 
    _ <- symbol "."
    r <- some digit
    return (JNumber (read (show l ++ "." ++ r)))

parseJScientific :: Parser JSON
parseJScientific = do
    l <- int 
    _ <- char '.'
    r <- int
    _ <- symbol "e" <|> symbol "E"
    sign <- symbol "+" <|> symbol "-"
    e <- int
    return (JNumber (read (show l ++ "." ++ show r ++ "e" ++ sign ++ show e)))

parseJArray :: Parser JSON
parseJArray = parseNonEmptyJArray 
-- <|> parseEmptyJArray

parseNonEmptyJArray :: Parser JSON
parseNonEmptyJArray = do
    _ <- symbol "["
    n <- parseJSON
    ns <- many (do 
        _ <- symbol "," 
        parseJSON)
    _ <- symbol "]"
    return (JArray (n:ns))

-- parseEmptyJArray :: Parser b
-- parseEmptyJArray = ???

parseJObj :: Parser JSON 
parseJObj = parseNonEmptyJObj
-- <|> parseEmptyJObj

parseNonEmptyJObj :: Parser JSON 
parseNonEmptyJObj = do
    _ <- symbol "{"
    n <- parseKV
    ns <- many (do
        _ <- symbol ","
        parseKV)
    _ <- symbol "}"
    return (JObj (n:ns))

parseKV :: Parser (String, JSON)
parseKV = do
        k <- parseString
        _ <- symbol ":"
        v <- parseJSON
        return (k,v)

-- parseEmptyJObj

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseJBoolean <|> parseJString <|> parseJNumber <|> parseJArray <|> parseJObj