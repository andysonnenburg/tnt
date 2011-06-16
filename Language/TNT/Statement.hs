module Language.TNT.Statement
       ( Statement (..)
       ) where

import Language.TNT.Expression

data Statement = Empty
               | Import String
               | Expression Expression deriving Show