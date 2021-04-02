module Jq.Json where

import Data.List

data JSON =
    JNull | JNumber Float | JString String | JBoolean Bool | JObj [(String, JSON)] | JArray [JSON]

instance Show JSON where
  -- adapted from "Pretty printing JSON data" exercise on weblab
  show JNull = "null"
  show (JNumber x) = show x
  show (JString x) = show x
  show (JBoolean True) = "true"
  show (JBoolean False) = "false"
  show (JArray x) = "[" ++ values x ++ "]" where
    values [] = ""
    values xs = intercalate ", " (map show xs)

  show (JObj x) = "{"++ renderList x ++ "}" where
    renderList ys = intercalate ", " ([show (fst y) ++ ": " ++ show (snd y) | y <- ys])

