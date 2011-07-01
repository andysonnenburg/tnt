module Language.TNT.Location
       ( Point (..)
       , Location (..)
       , Located (..)
       ) where

import Control.Comonad

import Data.Functor.Apply
import Data.Semigroup

data Point = Point Int Int deriving (Show, Eq, Ord)

data Location = Location Point Point deriving Show

data Located a = Locate Location a deriving Show

instance Semigroup Location where
  Location a b <> Location c d = Location (min a c) (max b d)
    
instance Functor Located where
  fmap f (Locate x a) = Locate x (f a)

instance Extend Located where
  duplicate w@(Locate x _) = Locate x w
  extend f w@(Locate x _) = Locate x (f w)

instance Comonad Located where
  extract (Locate _ a) = a

instance Apply Located where
  Locate x f <.> Locate y a = Locate (x <> y) (f a)