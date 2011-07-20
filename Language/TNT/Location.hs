{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  #-}
module Language.TNT.Location
       ( Point (..)
       , Location (..)
       , Located (..)
       ) where

import Control.Comonad

import Data.Data
import Data.Foldable
import Data.Functor.Apply
import Data.Semigroup
import Data.Traversable

data Point = Point Int Int deriving (Show, Eq, Ord, Data, Typeable)

data Location = Location Point Point deriving (Show, Data, Typeable)

data Located a = Locate Location a deriving ( Show
                                            , Functor
                                            , Foldable
                                            , Traversable
                                            , Data
                                            , Typeable
                                            )

instance Semigroup Location where
  Location a b <> Location c d = Location (min a c) (max b d)

instance Extend Located where
  duplicate w@(Locate x _) = Locate x w
  extend f w@(Locate x _) = Locate x (f w)

instance Comonad Located where
  extract (Locate _ a) = a

instance Apply Located where
  Locate x f <.> Locate y a = Locate (x <> y) (f a)