module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser

parseFilter :: Parser Filter
parseFilter = parseFArray <|> parsePipe <|> parseComma <|> parseFilterNoPipeComma

parseFilterNoComma :: Parser Filter
parseFilterNoComma = parsePipeNoComma <|> parseFilterNoPipeComma

parseFilterNoPipe :: Parser Filter
parseFilterNoPipe = parseCommaNoPipe <|> parseFilterNoPipeComma

parseFilterNoPipeComma :: Parser Filter 
parseFilterNoPipeComma = parseParenthesis <|> parseSugaredPipe <|>
  parseEmptyIteratorOpt <|> parseEmptyIterator <|>
  parseObjectValueIteratorOpt <|> parseObjectValueIterator <|> 
  parseArrayIteratorOpt <|> parseArrayIterator <|>
  parseArraySliceOpt <|> parseArraySlice 
  <|> parseArrayIndexOpt <|> parseArrayIndex 
  <|> parseOptIndex <|> parseIndex 
  <|> parseIdentity

parseFArray :: Parser Filter
parseFArray = do
  _ <- symbol "["
  f <- parseFilter
  _ <-symbol "]"
  return (FArray f)

parseSugaredPipe :: Parser Filter 
parseSugaredPipe =  do
    n <- parseOptIndex <|> parseIndex
    ns <- some (parseOptIndex <|> parseIndex)
    return (Pipe (n:ns))

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- symbol "("
  f <- parseFilter
  _ <- symbol ")"
  return (Parenthesis f)

parseComma :: Parser Filter 
parseComma = do
  x <- parseFilterNoComma
  xs <- some (do 
     _ <- symbol "," 
     parseFilterNoComma)
  return (Comma (x:xs))

parseCommaNoPipe :: Parser Filter 
parseCommaNoPipe = do
  x <- parseFilterNoPipeComma
  xs <- some (do 
     _ <- symbol "," 
     parseFilterNoPipeComma)
  return (Comma (x:xs))

parsePipe :: Parser Filter
parsePipe = do
  x <- parseFilterNoPipe
  xs <- some (do 
     _ <- symbol "|" 
     parseFilterNoPipe)
  return (Pipe (x:xs))

parsePipeNoComma :: Parser Filter
parsePipeNoComma = do
  x <- parseFilterNoPipeComma
  xs <- some (do 
     _ <- symbol "|" 
     parseFilterNoPipeComma)
  return (Pipe (x:xs))

parseIndex :: Parser Filter
parseIndex = do
  name <- parseIdentifierIndex <|> parseGenericIndex
  return (Indexing name)

parseOptIndex :: Parser Filter
parseOptIndex = do
  name <- parseIdentifierIndex <|> parseGenericIndex
  _ <- char '?'
  return (IndexingOpt  name)
  
parseArrayIndex :: Parser Filter
parseArrayIndex = do
  n <- parseArrayIndexValue
  return (ArrayIndex n)

parseArrayIndexOpt :: Parser Filter 
parseArrayIndexOpt = do
  n <- parseArrayIndexValue
  _ <- symbol "?"
  return (ArrayIndexOpt n)

parseArraySlice :: Parser Filter 
parseArraySlice = do
  (from, to) <- parseArraySliceBounds
  return (ArraySlice from to)

parseArraySliceOpt :: Parser Filter 
parseArraySliceOpt = do
  (from, to) <- parseArraySliceBounds
  _ <- symbol "?"
  return (ArraySliceOpt from to)

parseGenericIndex :: Parser String 
parseGenericIndex = do
  _ <- string ".["
  name <- parseString
  _ <- symbol "]"
  return name

parseEmptyIterator :: Parser Filter 
parseEmptyIterator = do
  _ <- symbol "."
  _ <- symbol "["
  _ <- symbol "]"
  return EmptyIterator

parseEmptyIteratorOpt :: Parser Filter 
parseEmptyIteratorOpt = do
  _ <- symbol "."
  _ <- symbol "["
  _ <- symbol "]"
  _ <- symbol "?"
  return EmptyIteratorOpt

parseArrayIterator :: Parser Filter 
parseArrayIterator = do
  indices <- parseArrayIteratorIndices
  return (ArrayIterator indices)

parseArrayIteratorOpt :: Parser Filter 
parseArrayIteratorOpt = do
  indices <- parseArrayIteratorIndices
  _ <- symbol "?"
  return (ArrayIteratorOpt indices)

parseObjectValueIterator :: Parser Filter 
parseObjectValueIterator = do 
  keys <- parseObjectValueIteratorKeys
  return (ObjectValueIterator keys)

parseObjectValueIteratorOpt :: Parser Filter 
parseObjectValueIteratorOpt = do 
  keys <- parseObjectValueIteratorKeys
  _ <- symbol "?"
  return (ObjectValueIteratorOpt keys)

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e

-- helper functions
parseArrayIndexValue :: Parser Int 
parseArrayIndexValue = do
  _ <- token . char $ '.'
  _ <- symbol "["
  n <- int
  _ <- symbol "]"
  return n

parseArrayIteratorIndices :: Parser [Int]
parseArrayIteratorIndices = do
  _ <- token . char $ '.'
  _ <- symbol "["
  n <- int
  ns <- many (do 
     _ <- symbol "," 
     int)
  _ <- symbol "]"
  return (n:ns)

parseObjectValueIteratorKeys :: Parser [String]
parseObjectValueIteratorKeys = do
  _ <- token . char $ '.'
  _ <- symbol "["
  n <- parseString
  ns <- many (do 
     _ <- symbol "," 
     parseString)
  _ <- symbol "]"
  return (n:ns)

parseArraySliceBounds :: Parser (Int, Int)
parseArraySliceBounds = do
  _ <- token . char $ '.'
  _ <- symbol "["
  from <- int
  _ <- symbol ":"
  to <- int
  _ <- symbol "]"
  return (from, to)

parseIdentifierIndex :: Parser String 
parseIdentifierIndex = do
  _ <- token . char $ '.'
  name <- ident <|> parseString
  return name