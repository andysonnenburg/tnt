{-# LANGUAGE RecordWildCards #-}
module Data.ClassFile.MethodInfo
       ( MethodInfo (..)
       , putMethodInfo
       ) where

import Control.Monad

import Data.Binary.Put
import Data.ClassFile.AttributeInfo
import Data.Word

data MethodInfo = MethodInfo
                  { accessFlags :: Word16
                  , nameIndex :: Word16
                  , descriptorIndex :: Word16
                  , attributes :: [AttributeInfo]
                  }

putMethodInfo :: MethodInfo -> Put
putMethodInfo MethodInfo {..} = do
  putWord16be accessFlags
  putWord16be nameIndex
  putWord16be descriptorIndex
  putWord16be . fromIntegral . length $ attributes
  forM_ attributes putAttributeInfo
  