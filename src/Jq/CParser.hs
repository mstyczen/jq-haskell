module Jq.CParser where

import Parsing.Parsing
    ( char,
      ident,
      int,
      parse,
      symbol,
      token,
      Alternative((<|>), some, many),
      Parser )
import Jq.Filters ( Config(ConfigC), Filter(..) )
import Jq.JParser ( parseString, parseJSON )
import Jq.Json ( JSON(JString) )

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseComma <|> parseBinaryOp <|> parseFilterNoPipeCommaEquals

parseFilterNoEquals :: Parser Filter
parseFilterNoEquals = parseFilterNoPipeCommaEquals

parseFilterNoComma :: Parser Filter
parseFilterNoComma = parsePipeNoComma <|> parseFilterNoPipeComma

parseFilterNoPipe :: Parser Filter
parseFilterNoPipe = parseCommaNoPipe <|> parseFilterNoPipeComma

parseFilterNoPipeComma :: Parser Filter 
parseFilterNoPipeComma = parseBinaryOp <|> parseFilterNoPipeCommaEquals

parseFilterNoPipeCommaEquals :: Parser Filter 
parseFilterNoPipeCommaEquals = parseTry <|> parseNot <|> parseIf <|> parseSugaredPipe <|> parseJSONConstructor <|> parseFArray <|> parseFDict <|> parseParenthesis <|> parseIdentifierIndexOpt <|> parseIdentifierIndex <|> parseBracketedFilter <|> parseRecDest <|> parseIdentity 

parseNot :: Parser Filter
parseNot = do 
  _ <- symbol "not"
  return Not

parseTry :: Parser Filter 
parseTry = do 
  _ <- symbol "try"
  try <- parseFilter
  x <- symbol "catch" <|> return "no_catch"
  if x=="catch" then do
    catch <- parseFilter
    return (Try try catch)
  else
    return (Try try Empty)
  


parseBinaryOp :: Parser Filter
parseBinaryOp = do
  left <- parseFilterNoEquals
  operator <- symbol "==" <|> symbol "!=" <|> symbol "<=" <|> symbol ">=" <|> symbol "<" <|> symbol ">" <|> symbol "and" <|> symbol "or"
  right <- parseFilterNoPipeComma
  case operator of
    "==" -> return (Equals left right) 
    "!=" -> return (NEquals left right)
    ">=" -> return (GeThEq left right)
    "<=" -> return (LeThEq left right)
    ">" -> return (GrTh left right)
    "<" -> return (LeTh left right)
    "and" -> return (And left right)
    _ -> return (Or left right)
    

parseRecDest :: Parser Filter
parseRecDest = do
  _ <- symbol ".."
  return RecursiveDescent

parseIf :: Parser Filter
parseIf = do
  _ <- symbol "if"
  condition <- parseFilter
  _ <- symbol "then"
  thenBranch <- parseFilter
  elseBranch <- parseSimpleElseBranch <|> parseElifBranch
  return (If condition thenBranch elseBranch)

parseSimpleElseBranch :: Parser Filter
parseSimpleElseBranch = do
  _ <- symbol "else"
  elseBranch <- parseFilter
  _ <- symbol "end"
  return elseBranch

parseElifBranch :: Parser Filter 
parseElifBranch = do
    _ <- symbol "el"
    parseIf


parseBracketedFilter :: Parser Filter
parseBracketedFilter = do 
  _ <- token . char $ '.'
  parseBracketedFilterNoComma
  

parseBracketedFilterNoComma :: Parser Filter
parseBracketedFilterNoComma = parseEmptyIteratorOpt <|> parseEmptyIterator <|>
  parseObjectValueIteratorOpt <|> parseObjectValueIterator <|> 
  parseArrayIteratorOpt <|> parseArrayIterator <|>
  parseArraySliceOpt <|> parseArraySlice 
  <|> parseArrayIndexOpt <|> parseArrayIndex 
  <|> parseGenericIndexOpt <|> parseGenericIndex

parseFDict :: Parser Filter
parseFDict = do
  _ <- symbol "{"
  kv <- parseOneKV
  kvs <- many (do 
     _ <- symbol "," 
     parseOneKV)
  _ <- symbol "}"
  return (FDict (kv:kvs))

parseOneKV :: Parser (Filter, Filter)
parseOneKV = do
  k <- parseFilter <|> do SimpleConstructor . JString <$> ident
  _ <- symbol ":"
  v <- parseFilter
  return (k,v) 
  <|> 
  do
  name <- ident <|> parseString
  return (SimpleConstructor $ JString name, Indexing name)

parseFArray :: Parser Filter
parseFArray = do
  _ <- symbol "["
  f <- parseFilter
  _ <-symbol "]"
  return (FArray f)

parseSugaredPipe :: Parser Filter 
parseSugaredPipe =  do
    n <- parseIdentifierIndexOpt <|> parseIdentifierIndex <|> parseBracketedFilter
    ns <- some (parseIdentifierIndexOpt <|> parseIdentifierIndex <|>  parseBracketedFilterNoComma)
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
     parseFilterNoPipe)
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
     parseFilter)
  return (Pipe (x:xs))

parsePipeNoComma :: Parser Filter
parsePipeNoComma = do
  x <- parseFilterNoPipeComma
  xs <- some (do 
     _ <- symbol "|" 
     parseFilter)
  return (Pipe (x:xs))

parseGenericIndexOpt :: Parser Filter
parseGenericIndexOpt = do
  _ <- symbol "["
  name <- parseString
  _ <- symbol "]"
  return (IndexingOpt name)

parseIdentifierIndexOpt :: Parser Filter
parseIdentifierIndexOpt = do
  _ <- token . char $ '.'
  name <- ident <|> parseString
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

parseGenericIndex :: Parser Filter 
parseGenericIndex = do
  _ <- symbol "["
  name <- parseString
  _ <- symbol "]"
  return (Indexing name)

parseEmptyIterator :: Parser Filter 
parseEmptyIterator = do
  _ <- symbol "["
  _ <- symbol "]"
  return EmptyIterator

parseEmptyIteratorOpt :: Parser Filter 
parseEmptyIteratorOpt = do
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
  _ <- symbol "["
  n <- int
  _ <- symbol "]"
  return n

parseArrayIteratorIndices :: Parser [Int]
parseArrayIteratorIndices = do
  _ <- symbol "["
  n <- int
  ns <- many (do 
     _ <- symbol "," 
     int)
  _ <- symbol "]"
  return (n:ns)

parseObjectValueIteratorKeys :: Parser [String]
parseObjectValueIteratorKeys = do
  _ <- symbol "["
  n <- parseString
  ns <- many (do 
     _ <- symbol "," 
     parseString)
  _ <- symbol "]"
  return (n:ns)

parseArraySliceBounds :: Parser (Int, Int)
parseArraySliceBounds = do
  _ <- symbol "["
  from <- int <|> return 0
  _ <- symbol ":"
  to <- int <|> return 2147483647
  _ <- symbol "]"
  return (from, to)

parseIdentifierIndex :: Parser Filter 
parseIdentifierIndex = do
  _ <- token . char $ '.'
  name <- ident <|> parseString
  return (Indexing name)

parseJSONConstructor :: Parser Filter 
parseJSONConstructor = do
  json <- parseJSON
  return (SimpleConstructor json) 
