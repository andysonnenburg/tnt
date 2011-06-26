module Language.TNT.Location
       ( Point (..)
       , Location (..)
       , Located (..)
       ) where

import Data.Monoid

data Point = Point Int Int deriving Show

data Location = Location Point Point deriving Show

data Located a = Located Location a deriving Show

instance Monoid Location where
  mempty = Location (Point 0 0) (Point 0 0)
  mappend
    (Location (Point x1 y1) (Point x2 y2))
    (Location (Point x3 y3) (Point x4 y4)) =
      Location (Point x1' y1') (Point x2' y2')
    where
      (y1', x1') = min (y1, x1) (y3, x3)
      (y2', x2') = max (y2, x2) (y4, x4)
    
    