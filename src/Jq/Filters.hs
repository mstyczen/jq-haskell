{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Filters where

data Filter = Identity 
  | Indexing String | IndexingOpt String 
  | ArrayIndex Int | ArrayIndexOpt Int
  | ArraySlice Int Int | ArraySliceOpt Int Int 
  | EmptyIterator | EmptyIteratorOpt
  | ArrayIterator [Int] | ArrayIteratorOpt [Int]
  | ObjectValueIterator [String] | ObjectValueIteratorOpt [String]
  | Comma [Filter] | Pipe [Filter]
  | Parenthesis Filter


instance Show Filter where
  show (Identity) = "identity ."
  -- show (Parenthesis) = "parenthesis ()"
  show (Indexing x) = "obj-index ." ++ x
  show (IndexingOpt x) = "opt-obj-index ." ++ x

data Config = ConfigC {filters :: Filter}
