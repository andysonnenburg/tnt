{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Language.TNT.Unique.Class
       ( MonadUnique (..)
       ) where

class MonadUnique u m | m -> u where
  newUnique :: m u