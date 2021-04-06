{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Json where

import Data.List
import Data.Char

data JSON =
    JNull | JNumber Float | JString String | JBoolean Bool | JObj [(String, JSON)] | JArray [JSON]

instance Show JSON where
  -- adapted from "Pretty printing JSON data" exercise on weblab
  show JNull = "null"
  show (JNumber x) = if x == fromInteger (round x)
    then show (fromInteger(round x)) else show x

  show (JString x) = showMyString ("\"" ++ x ++ "\"")
  show (JBoolean True) = "true"
  show (JBoolean False) = "false"
  show (JArray xs) = showArray xs 1
  show (JObj xs) = showDict xs 1

instance Eq JSON where
  (==) (JNumber a) (JNumber b) = a==b 
  (==) (JString a) (JString b) = a==b
  (==) (JBoolean a) (JBoolean b) = a==b
  (==) (JArray a) (JArray b) = a==b
  (==) (JObj a) (JObj b) = a==b
  (==) JNull JNull = True 
  (==) _ _ = False

instance Ord JSON where
  (<=) (JNumber a) (JNumber b) = a<=b 
  (<=) (JString a) (JString b) = a<=b
  (<=) (JBoolean a) (JBoolean b) = a<=b
  (<=) (JArray a) (JArray b) = a<=b
  (<=) (JObj a) (JObj b) = a<=b
  (<=) JNull JNull = True 
  (<=) JNull _ = True
  (<=) _ JNull = False
  (<=) (JBoolean _) _ = True
  (<=) _ (JBoolean _) = False
  (<=) (JNumber _) _ = True
  (<=) _ (JNumber _) = False
  (<=) (JString _) _ = True
  (<=) _ (JString _) = False
  (<=) (JArray _) _ = True
  (<=) _ (JArray _) = False


showArray :: [JSON] -> Int -> String
showArray [] _= "[]"
showArray xs n =
  "[\n" ++ spaces n ++ values xs ++ "\n" ++ spaces (n-1) ++ "]" where
    values [] = ""
    values arr = intercalate (",\n"++spaces n) (map (\x -> case x of
      JArray vs -> showArray vs (n+1)
      JObj vs -> showDict vs (n+1)
      other -> show other) arr)

spaces :: Int -> [Char]
spaces n = take (2*n) $ repeat ' '

showDict :: [(String, JSON)] -> Int -> String
showDict [] _ = "{}"
showDict xs n = 
  "{\n" ++ spaces n ++ kvpairs xs ++ "\n" ++ spaces (n-1) ++ "}" where
    kvpairs [] = ""
    kvpairs arr = intercalate (",\n"++spaces n) (map (\x -> 
      show (fst x) ++ ": " ++
      case snd x of
        JArray vs -> showArray vs (n+1)
        JObj vs -> showDict vs (n+1)
        other -> show other) arr)
  
showMyString :: String -> String
showMyString [] = []
showMyString (x:xs) = if x < '\255' then showLitChar x (showMyString xs)
else x : showMyString xs

