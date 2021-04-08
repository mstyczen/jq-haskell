module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

-- dict indexing
compile (Indexing field) (JObj elements) = Right [dictLookup field elements]
compile (Indexing _) JNull = Right [JNull]
compile (Indexing _) _ = Left "Dictionary indexing operator applied to a non-dict argument."


compile (IndexingOpt field) (JObj elements) = Right [dictLookup field elements]
compile (IndexingOpt _) JNull = Right [JNull]
compile (IndexingOpt _) _ = return []

-- array indexing and slicing
compile (ArrayIndex n) (JArray arr) = Right [arrayLookup n arr]
compile (ArrayIndex _) JNull = Right [JNull]
compile (ArrayIndex _) _ = Left "Array indexing operator applied to a non-array argument."

compile (ArrayIndexOpt n) (JArray arr) = Right [arrayLookup n arr]
compile (ArrayIndexOpt _) JNull = Right [JNull]
compile (ArrayIndexOpt _) _ = return []

compile (ArraySlice from to) (JArray arr) = Right [JArray (slice from to arr)]
compile (ArraySlice from to) (JString str) = Right [JString (slice from to str)]
compile (ArraySlice _ _) JNull = Right [JNull]
compile (ArraySlice _ _) _ = Left "Array slicing operator applied to a non-array argument."

compile (ArraySliceOpt from to) (JArray arr) = Right [JArray (slice from to arr)]
compile (ArraySliceOpt from to) (JString str) = Right [JString (slice from to str)]
compile (ArraySliceOpt _ _) JNull = Right [JNull]
compile (ArraySliceOpt _ _) _ = return []

-- iterators
compile (ArrayIterator indices) (JArray arr) = Right (getByIndices indices arr)
compile (ArrayIterator indices) JNull = Right [JNull | _ <- [1..(length indices)]]
compile (ArrayIterator _) _ = Left "Array iterator operator applied to a non-array argument."

compile (ArrayIteratorOpt indices) (JArray arr) = Right (getByIndices indices arr)
compile (ArrayIteratorOpt indices) JNull = Right [JNull | _ <- [1..(length indices)]]
compile (ArrayIteratorOpt _) _ = return []

compile (ObjectValueIterator keys) (JObj dict) = Right (getByKeys keys dict) 
compile (ObjectValueIterator keys) JNull = Right [JNull | _ <- [1..(length keys)]]
compile (ObjectValueIterator _) _ = Left "Object value iterator operator applied to a non-dict argument."

compile (ObjectValueIteratorOpt keys) (JObj dict) = Right (getByKeys keys dict)
compile (ObjectValueIteratorOpt keys) JNull = Right [JNull | _ <- [1..(length keys)]] 
compile (ObjectValueIteratorOpt _) _ = return []

compile EmptyIterator (JArray arr) = Right arr
compile EmptyIterator (JObj xs) = Right [snd x | x <- xs]
compile EmptyIterator _ = Left "Empty iterator operator applied to a non-array/dict argument."

compile EmptyIteratorOpt (JArray arr) = Right arr
compile EmptyIteratorOpt (JObj xs) = Right [snd x | x <- xs]
compile EmptyIteratorOpt _ = Right []

-- comma and pipe
compile (Comma fs) argument = concatenateResults [compile f argument | f <- fs]
compile (Pipe fs) argument = applySequentially fs [argument]

-- parenthesis
compile (Parenthesis f) argument = compile f argument

-- empty
compile Empty _ = return []

compile (Try try catch) argument = case compile try argument of
    Left _ -> compile catch argument
    Right xs -> Right xs

-- FArray
compile (FArray f) argument = do
    x <- compile f argument
    return [JArray x]

-- simple constructor
compile (SimpleConstructor json) _ = Right [json]

-- FDict
compile (FDict []) _ = Right [JObj []]
compile (FDict (x:xs)) argument = do
    keys <- compile keysFs argument
    values <- compile valsFs argument
    cartesian (kvpairs keys values) (compile (FDict xs) argument)
    where
        keysFs = fst x
        valsFs = snd x

-- recursive descent
compile RecursiveDescent JNull = Right [JNull]
compile RecursiveDescent (JNumber x) = Right [JNumber x]
compile RecursiveDescent (JBoolean x) = Right [JBoolean x]
compile RecursiveDescent (JArray []) = Right [JArray []]
compile RecursiveDescent (JObj []) = Right [JObj []]
compile RecursiveDescent x = do
    ys <- compile EmptyIterator x
    yss <- recDescList ys
    return (x : yss)

-- equals and nequals
compile (Equals left right) argument = do
    xs <- compile left argument
    ys <- compile right argument
    return [JBoolean (x == y) | x <- xs, y <- ys]

compile (NEquals left right) argument = do
    xs <- compile left argument
    ys <- compile right argument
    return [JBoolean (x /= y) | x <- xs, y <- ys]

compile (GeThEq left right) argument = do
    xs <- compile left argument
    ys <- compile right argument
    return [JBoolean (x >= y) | x <- xs, y <- ys]

compile (LeThEq left right) argument = do
    xs <- compile left argument
    ys <- compile right argument
    return [JBoolean (x <= y) | x <- xs, y <- ys]
    
compile (LeTh left right) argument = do
    xs <- compile left argument
    ys <- compile right argument
    return [JBoolean (x < y) | x <- xs, y <- ys]

compile (GrTh left right) argument = do
    xs <- compile left argument
    ys <- compile right argument
    return [JBoolean (x > y) | x <- xs, y <- ys]

compile (If condition thenBranch elseBranch) argument = do
    conditionValues <- compile condition argument
    thenValues <- compile thenBranch argument
    elseValues <- compile elseBranch argument
    Right $ concat $ map (\x -> case x of 
        JNull -> elseValues
        (JBoolean False) -> elseValues
        _ -> thenValues) conditionValues
    
-- and / or / not
compile (And left right) argument = do
    leftVs <- compile left argument
    rightVs <- compile right argument
    Right $ concat $ map (\x -> if not x then [JBoolean False] else [JBoolean v | v <- (map toBoolean rightVs)]) (map toBoolean leftVs)

compile (Or left right) argument = do
    leftVs <- compile left argument
    rightVs <- compile right argument
    Right $ concat $ map (\x -> if x then [JBoolean True] else [JBoolean v | v <- (map toBoolean rightVs)]) (map toBoolean leftVs)

compile Not argument = return [(JBoolean (not $ toBoolean argument))]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

-- helper functions
applySequentially :: [Filter] -> [JSON] -> Either String [JSON]
applySequentially [] arguments = Right arguments
-- applySequentially [f] arguments = convertToSingle [compile f argument | argument <- arguments]
applySequentially (f:fs) arguments = case convertToSingle [compile f argument | argument <- arguments] of
    Left err -> Left err
    Right [] -> Right []
    Right xs ->  applySequentially fs xs 

convertToSingle :: [Either String [JSON]] -> Either String [JSON]
convertToSingle [] = Right []
convertToSingle (x:xs) = case x of 
    Left err -> Left err
    Right v -> case convertToSingle(xs) of
        Left err2 -> Left err2
        Right vs -> Right (v ++ vs)                    


concatenateResults :: [Either String [JSON]] -> Either String [JSON]
concatenateResults [] = Right []
concatenateResults (x:xs) = case concatenateResults xs of 
    Left error1 -> Left error1
    Right ys -> case x of 
        Left error2 -> Left error2
        Right y -> Right (y ++ ys)


dictLookup :: String -> [(String, JSON)] -> JSON
dictLookup _ [] = JNull
dictLookup field (x:xs) = if show field == show (fst x) then snd x else dictLookup field xs

arrayLookup :: Int -> [JSON] -> JSON
arrayLookup n arr = if index > length arr then JNull 
    else arr !! index where 
        index = convIndex n arr

slice :: Int -> Int -> [a] -> [a]
slice from to xs =
    if convTo <= convFrom then []
    else take (end - start) (drop start xs)
        where
            convFrom = convIndex from xs
            convTo = convIndex to xs
            start = max 0 convFrom
            end = min (length xs) convTo


getByIndices :: [Int] -> [JSON] -> [JSON]
getByIndices indices xs = map (getByIndex xs) indices

getByIndex :: [JSON] -> Int -> JSON 
getByIndex arr index = 
    if cindex >= 0 && cindex < length arr then arr !! cindex
    else JNull where cindex = convIndex index arr

convIndex :: Foldable t => Int -> t a -> Int
convIndex index xs = if index >= 0 then index else length xs + index

getByKeys :: [String] -> [(String, JSON)] -> [JSON]
getByKeys keys xs = map (\key -> getByKey key xs) keys

getByKey :: String -> [(String, JSON)] -> JSON
getByKey _ [] = JNull 
getByKey key (x:xs) = if fst x == key then snd x else getByKey key xs

recDescList :: [JSON] -> Either String [JSON]
recDescList [] = Right []
recDescList (x:xs) = case compile RecursiveDescent x of
    Right ls -> do
        lss <- recDescList xs
        return (ls ++ lss)
    Left err -> Left err

cartesian :: Either String [JSON] -> Either String [JSON] -> Either String [JSON]
cartesian mxs mys = do
    xs <- mxs
    ys <- mys
    return [concatDict x y | x <-xs, y<-ys]


kvpairs :: [JSON] -> [JSON] -> Either String [JSON]
kvpairs keys values = case keysToStrings keys of
    Right stringkeys -> Right [(JObj [(k,v)]) | k <- stringkeys, v <- values]
    Left err -> Left err


keysToStrings :: [JSON] -> Either String [String]
keysToStrings [] = Right []
keysToStrings (x:xs) = case x of 
    JString s -> do 
        ss <- keysToStrings xs
        return (s:ss)
    _ -> Left "non-string dict key"


concatDict :: JSON -> JSON -> JSON
concatDict (JObj x) (JObj y) = JObj (x ++ y)