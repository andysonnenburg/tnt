{-# LANGUAGE RecordWildCards #-}
module Data.ClassFile.MethodInfo
       ( MethodInfo (..)
       , putMethodInfo
       ) where

import Control.Monad

import Data.Binary
import Data.Binary.Put
import Data.ClassFile.Access
import Data.ClassFile.AttributeInfo

data MethodInfo = MethodInfo
                  { accessFlags :: MethodAccess
                  , nameIndex :: Word16
                  , descriptorIndex :: Word16
                  , attributes :: [AttributeInfo]
                  }

putMethodInfo :: MethodInfo -> Put
putMethodInfo MethodInfo {..} = do
  put accessFlags
  putWord16be nameIndex
  putWord16be descriptorIndex
  putWord16be . fromIntegral . length $ attributes
  forM_ attributes putAttributeInfo
  