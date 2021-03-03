module Jq.Json where

data JSON =
    JNull

instance Show JSON where
  show (JNull) = "null"
