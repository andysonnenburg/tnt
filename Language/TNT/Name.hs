{-# LANGUAGE DeriveDataTypeable #-}
module Language.TNT.Name
       ( Name (..)
       ) where

import Data.Data

import Language.TNT.Unique

data Name = Name Unique String deriving (Show, Data, Typeable)

instance Eq Name where
  Name x _ == Name y _ = x == y

instance Ord Name where
  compare (Name x _) (Name y _) = compare x y
