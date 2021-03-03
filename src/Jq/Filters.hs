module Jq.Filters where

data Filter = Identity

instance Show Filter where
  show (Identity) = "."

data Config = ConfigC {filters :: Filter}
