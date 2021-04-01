module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

-- dict indexing
compile (Indexing field) (JObj elements) = Right [dictLookup field elements]
compile (Indexing _) _ = Left "Dictionary indexing operator applied to a non-dict argument."

compile (IndexingOpt field) (JObj elements) = Right [dictLookup field elements]
-- not sure if you should return null or empty list, but jqplay suggests that it should be empty list
compile (IndexingOpt _) _ = return []

-- array indexing and slicing
compile (ArrayIndex n) (JArray arr) = Right [arrayLookup n arr]
compile (ArrayIndex _) _ = Left "Array indexing operator applied to a non-array argument."

compile (ArrayIndexOpt n) (JArray arr) = Right [arrayLookup n arr]
compile (ArrayIndexOpt _) _ = return []

compile (ArraySlice from to) (JArray arr) = Right [JArray (slice from to arr)]
compile (ArraySlice _ _) _ = Left "Array slicing operator applied to a non-array argument."

compile (ArraySliceOpt from to) (JArray arr) = Right [JArray (slice from to arr)]
compile (ArraySliceOpt _ _) _ = return []

-- iterators
compile (ArrayIterator indices) (JArray arr) = Right (getByIndices indices arr)
compile (ArrayIterator _) _ = Left "Array iterator operator applied to a non-array argument."

compile (ArrayIteratorOpt indices) (JArray arr) = Right (getByIndices indices arr)
compile (ArrayIteratorOpt _) _ = return []

compile (ObjectValueIterator keys) (JObj dict) = Right (getByKeys keys dict) 
compile (ObjectValueIterator _) _ = Left "Object value iterator operator applied to a non-dict argument."

compile (ObjectValueIteratorOpt keys) (JObj dict) = Right (getByKeys keys dict) 
compile (ObjectValueIteratorOpt _) _ = return []

compile EmptyIterator (JArray arr) = Right arr
compile EmptyIterator (JObj xs) = Right [snd x | x <- xs]
compile EmptyIterator _ = Left "Empty iterator operator applied to a non-array/dict argument."

compile EmptyIteratorOpt (JArray arr) = Right arr
compile EmptyIteratorOpt (JObj xs) = Right [snd x | x <- xs]
compile EmptyIteratorOpt _ = Left "Empty iterator operator applied to a non-array/dict argument."


run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j


-- helper functions
dictLookup :: String -> [(String, JSON)] -> JSON
dictLookup _ [] = JNull
dictLookup field (x:xs) = if show field == show (fst x) then snd x else dictLookup field xs

arrayLookup :: Int -> [JSON] -> JSON
arrayLookup n arr = if index > length arr then JNull 
    else arr !! index where 
        index = convIndex n arr

slice :: Int -> Int -> [JSON] -> [JSON]
slice from to xs =
    if convTo <= convFrom then []
    else take (end - start) (drop start xs)
        where
            convFrom = convIndex from xs
            convTo = convIndex to xs
            start = max 0 convFrom
            end = min (length xs) convTo


getByIndices :: [Int] -> [JSON] -> [JSON]
getByIndices indices xs = map (\i -> getByIndex xs i) indices

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
