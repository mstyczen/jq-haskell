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
  show (JArray x) = "[\n " ++ values x ++ "\n]" where
    values [] = ""
    values xs = intercalate ",\n " (map show xs)

  show (JObj x) = "{"++ renderList x ++ "}" where
    renderList ys = intercalate "," ([show (fst y) ++ ":" ++ show (snd y) | y <- ys])


showMyString :: String -> String
showMyString [] = []
showMyString (x:xs) = if x < '\255' then showLitChar x (showMyString xs)
else x : showMyString xs

