module Language.TNT.Expression
       ( Expression (..)
       ) where

data Expression = Variable String
                | String String
                | Declare String Expression
                | Access Expression String
                | Mutate Expression String Expression
                | Assign String Expression
                | Invoke Expression [Expression] deriving Show