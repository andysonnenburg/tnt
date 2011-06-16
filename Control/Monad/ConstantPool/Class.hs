{-# LANGUAGE TypeFamilies #-}
module Control.Monad.ConstantPool.Class (MonadConstantPool (..)) where

import Data.Int
import Data.Word

class Monad m => MonadConstantPool m where
  
  type Table m
  
  getCount :: m Word16
  getTable :: m (Table m)
  lookupClass :: String -> m Word16
  lookupField :: String -> String -> String -> m Word16
  lookupMethod :: String -> String -> String -> m Word16
  lookupInterfaceMethod :: String -> String -> String -> m Word16
  lookupString :: String -> m Word16
  lookupInteger :: Int32 -> m Word16
  lookupFloat :: Float -> m Word16
  lookupLong :: Int64 -> m Word16
  lookupDouble :: Double -> m Word16
  lookupUtf8 :: String -> m Word16