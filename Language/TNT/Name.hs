module Language.TNT.Name
       ( Name (..)
       ) where

import Language.TNT.Unique

data Name = Name Unique Unique String deriving Show
