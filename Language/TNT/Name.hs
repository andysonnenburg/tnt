{-# LANGUAGE DeriveDataTypeable #-}
module Language.TNT.Name
       ( Name (..)
       ) where

import Data.Data

import Language.TNT.Unique

data Name = Top
          | Nested Name Unique String deriving (Show, Data, Typeable)

instance Eq Name where
  Top == Top = True
  Nested _ x _ == Nested _ y _ = x == y
  _ == _ = False

instance Ord Name where
  Top <= _ = True
  Nested _ x _ <= Nested _ y _ = x <= y
  Nested _ _ _ <= Top = False
