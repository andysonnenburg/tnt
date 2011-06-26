module Language.TNT.Message
       ( Message (..)
       ) where

import Language.TNT.Location

data Message = Message Location String deriving Show