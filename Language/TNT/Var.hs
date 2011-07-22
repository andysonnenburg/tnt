{-# LANGUAGE DeriveDataTypeable #-}
module Language.TNT.Var
       ( Var (..)
       , VarSort (..)
       ) where

import Data.Data

import Language.TNT.Name
import Language.TNT.Unique

data Var = Var VarSort (Maybe Name) Name deriving (Show, Data, Typeable)

data VarSort = Ref | Val | Fun deriving (Show, Eq, Data, Typeable)