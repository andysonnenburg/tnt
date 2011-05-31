{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Data.ClassFile.CodeAttribute (CodeAttribute (..)) where

import Control.Monad
import Control.Monad.ConstantPool.Class

import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.ClassFile.Attribute
import Data.ClassFile.AttributeInfo
import Data.Word

data CodeAttribute = CodeAttribute
                     { maxStack :: Word16
                     , maxLocals :: Word16
                     , code :: ByteString
                     , exceptionTable :: [ExceptionTableEntry]
                     , attributes :: [SomeAttribute]
                     }

data ExceptionTableEntry = ExceptionTableEntry
                           { startPc :: Word16
                           , endPc :: Word16
                           , handlerPc :: Word16
                           , catchType :: Word16
                           }

instance Attribute CodeAttribute where
  toAttributeInfo CodeAttribute {..} = do
    attributeNameIndex <- lookupUtf8 "Code"
    let m = do
          putWord16be maxStack
          putWord16be maxLocals
          putWord32be . fromIntegral . BL.length $ code
          putLazyByteString code
          putWord16be . fromIntegral . length $ exceptionTable
          forM_ exceptionTable putExceptionTableEntry
          putWord16be . fromIntegral . length $ attributes
    xs <- forM attributes toAttributeInfo
    let info = runPut (m >> forM_ xs putAttributeInfo)
    return AttributeInfo { attributeNameIndex
                         , info
                         }
  
putExceptionTableEntry :: ExceptionTableEntry -> Put
putExceptionTableEntry ExceptionTableEntry {..} = do
  putWord16be startPc
  putWord16be endPc
  putWord16be handlerPc
  putWord16be catchType