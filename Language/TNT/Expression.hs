module Language.TNT.Expression
       ( Expression (..)
       ) where

data Expression = Empty
                | Import String
                | Variable String
                | String String
                | Declare String Expression
                | Access Expression String
                | Mutate Expression Expression
                | Invoke Expression [Expression] deriving Show