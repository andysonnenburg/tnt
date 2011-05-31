{-# LANGUAGE RecordWildCards #-}
module Data.ClassFile
       ( ClassFile (..)
       , putClassFile
       ) where

import Control.Monad

import Data.Binary.Put
import Data.ClassFile.AttributeInfo
import Data.ClassFile.CpInfo
import Data.ClassFile.FieldInfo
import Data.ClassFile.MethodInfo
import Data.Word

data ClassFile = ClassFile
                 { minorVersion :: Word16
                 , majorVersion :: Word16
                 , constantPoolLength :: Word16
                 , constantPool :: [CpInfo]
                 , accessFlags :: Word16
                 , thisClass :: Word16
                 , superClass :: Word16
                 , interfaces :: [Word16]
                 , fields :: [FieldInfo]
                 , methods :: [MethodInfo]
                 , attributes :: [AttributeInfo]
                 }

putClassFile :: ClassFile -> Put
putClassFile ClassFile {..} = do
  putWord32be 0xCAFEBABE
  putWord16be minorVersion
  putWord16be majorVersion
  putWord16be constantPoolLength
  forM_ constantPool putCpInfo
  putWord16be accessFlags
  putWord16be thisClass
  putWord16be superClass
  putWord16be . fromIntegral . length $ interfaces
  forM_ interfaces putWord16be
  putWord16be . fromIntegral . length $ fields
  forM_ fields putFieldInfo
  putWord16be . fromIntegral . length $ methods
  forM_ methods putMethodInfo
  putWord16be . fromIntegral . length $ attributes
  forM_ attributes putAttributeInfo