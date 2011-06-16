{-# LANGUAGE RecordWildCards #-}
module Data.ClassFile.FieldInfo
       ( FieldInfo (..)
       , putFieldInfo
       ) where

import Control.Monad

import Data.Binary
import Data.Binary.Put
import Data.ClassFile.Access
import Data.ClassFile.AttributeInfo

data FieldInfo = FieldInfo
                 { accessFlags :: FieldAccess
                 , nameIndex :: Word16
                 , descriptorIndex :: Word16
                 , attributes :: [AttributeInfo]
                 }

putFieldInfo :: FieldInfo -> Put
putFieldInfo FieldInfo {..} = do
  put accessFlags
  putWord16be nameIndex
  putWord16be descriptorIndex
  putWord16be . fromIntegral . length $ attributes
  forM_ attributes putAttributeInfo