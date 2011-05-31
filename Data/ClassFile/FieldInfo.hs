{-# LANGUAGE RecordWildCards #-}
module Data.ClassFile.FieldInfo
       ( FieldInfo (..)
       , putFieldInfo
       ) where

import Control.Monad

import Data.Binary.Put
import Data.ClassFile.AttributeInfo
import Data.Word

data FieldInfo = FieldInfo
                 { accessFlags :: Word16
                 , nameIndex :: Word16
                 , descriptorIndex :: Word16
                 , attributes :: [AttributeInfo]
                 }

putFieldInfo :: FieldInfo -> Put
putFieldInfo FieldInfo {..} = do
  putWord16be accessFlags
  putWord16be nameIndex
  putWord16be descriptorIndex
  putWord16be . fromIntegral . length $ attributes
  forM_ attributes putAttributeInfo