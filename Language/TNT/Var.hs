{-# LANGUAGE DeriveDataTypeable #-}
module Language.TNT.Var
       ( Var (..)
       , VarSort (..)
       ) where

import Language.TNT.Unique

data Var = Var VarSort Unique String deriving (Show, Data, Typeable)

data VarSort = Ref | Val deriving (Show, Data, Typeable)