{-# LANGUAGE RecordWildCards #-}
module Data.ClassFile.AttributeInfo
       ( AttributeInfo (..)
       , putAttributeInfo
       ) where

import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Word

data AttributeInfo = AttributeInfo
                     { attributeNameIndex :: Word16
                     , info :: ByteString
                     }

putAttributeInfo :: AttributeInfo -> Put
putAttributeInfo AttributeInfo {..} = do
  putWord16be attributeNameIndex
  putWord32be . fromIntegral . BL.length $ info
  putLazyByteString info