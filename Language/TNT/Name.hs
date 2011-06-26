module Language.TNT.Name (Name (..)) where

import Language.TNT.Unique

data Name = Name Unique String deriving Show