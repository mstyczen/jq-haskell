{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Filters where
import Jq.Json ( JSON )
data Filter = Identity 
  | Indexing String | IndexingOpt String 
  | ArrayIndex Int | ArrayIndexOpt Int
  | ArraySlice Int Int | ArraySliceOpt Int Int 
  | EmptyIterator | EmptyIteratorOpt
  | ArrayIterator [Int] | ArrayIteratorOpt [Int]
  | ObjectValueIterator [String] | ObjectValueIteratorOpt [String]
  | Comma [Filter] | Pipe [Filter]
  | Parenthesis Filter | FArray Filter | FDict [(Filter, Filter)]
  | SimpleConstructor JSON
  | RecursiveDescent
  | Equals Filter Filter | NEquals Filter Filter
  | GrTh Filter Filter | LeTh Filter Filter
  | LeThEq Filter Filter | GeThEq Filter Filter
  | If Filter Filter Filter
  | And Filter Filter
  | Or Filter Filter
  | Not Filter
instance Show Filter where
  show (Identity) = "identity ."
  -- show (Parenthesis) = "parenthesis ()"
  show (Indexing x) = "obj-index ." ++ x
  show (IndexingOpt x) = "opt-obj-index ." ++ x

data Config = ConfigC {filters :: Filter}
