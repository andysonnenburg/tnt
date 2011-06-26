{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.TNT.Unique
       ( Unique
       , UniqueM
       , runUniqueM
       , newUnique
       ) where

import Control.Applicative
import Control.Monad.State

newtype Unique = Unique Int deriving Show

newtype UniqueM a = UniqueM
                    { unUniqueM :: State Int a
                    } deriving ( Functor
                               , Applicative
                               , Monad
                               )

runUniqueM :: UniqueM a -> a
runUniqueM = flip evalState 0 . unUniqueM

newUnique :: UniqueM Unique
newUnique = UniqueM $ do
  i <- get
  put $ i + 1
  return . Unique $ i