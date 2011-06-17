{-# LANGUAGE StandaloneDeriving #-}
module Data.Graph.Extra () where

import Data.Graph

deriving instance Show a => Show (SCC a)