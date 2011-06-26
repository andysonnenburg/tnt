module Language.TNT.Var (Var (..)) where

import Language.TNT.Unique

data Var = Global String
         | Nonlocal Unique String
         | Local String deriving Show