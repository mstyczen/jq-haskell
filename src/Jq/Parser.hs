module Jq.Parser (module Jq.JParser, module Jq.CParser, parse) where

import qualified Parsing.Parsing as P (Parser, parse)
import Jq.JParser
import Jq.CParser

parse :: P.Parser a -> String -> Maybe a
parse p s = case (P.parse p s) of
  [(v, "")] -> Just v
  _ -> Nothing
