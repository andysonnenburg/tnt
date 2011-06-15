module Language.TNT.Position
       ( Position (..)
       ) where

data Position = (:+:) { lineNumber :: {-# UNPACK #-} !Int
                      , columnNumber :: {-# UNPACK #-} !Int
                      } deriving Show