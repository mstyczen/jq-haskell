module Jq.JParser where

import Parsing.Parsing
    ( char,
      digit,
      int,
      item,
      sat,
      string,
      symbol,
      token,
      Alternative((<|>), some, many),
      Parser )
import Jq.Json ( JSON(..) )
import Data.Char ( readLitChar )

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

parseEmptyJObj :: Parser JSON 
parseEmptyJObj = do
    _ <- string "{}"
    return(JObj [])

parseEmptyJArray :: Parser JSON 
parseEmptyJArray = do
    _ <- string "[]"
    return (JArray [])


parseString :: Parser String 
parseString = do
    _ <- symbol "\""
    s <- many (parseEscape <|> sat (\x -> x /= '"' && x /= '\\'))
    _ <- symbol "\""
    return s

parseEscape :: Parser Char
parseEscape = do
    _ <- char '\\'
    parseUnicode <|> parseOtherEsc

parseOtherEsc :: Parser Char
parseOtherEsc = do
    x <- item
    return (fst $ head $ readLitChar ("\\"++[x]))
    -- case x of
    --     'n' -> return ('\n')
    --     't' -> return ('\t')
    --     'r' -> return ('\r')
    --     'f' -> return ('\f')
    --     'b' -> return ('\b')
        
parseUnicode :: Parser Char
parseUnicode = do
    _ <- char 'u'
    d1 <- parseHexDigit
    d2 <- parseHexDigit
    d3 <- parseHexDigit
    d4 <- parseHexDigit
    return (fst $ head $ readLitChar ("\\" ++ show (read ("0x" ++ [d1,d2,d3,d4]) :: Int)))

parseHexDigit :: Parser Char
parseHexDigit = sat (`elem` "abcdefABCDEF") <|> digit

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
parseJArray = parseEmptyJArray <|> parseNonEmptyJArray 
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
parseJObj = parseEmptyJObj <|> parseNonEmptyJObj
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