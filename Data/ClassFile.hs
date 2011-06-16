{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Data.ClassFile
       ( ClassFile (..)
       , putClassFile
       , classM
       , methodM
       , codeAttributeM
       ) where

import Control.Monad
import Control.Monad.ConstantPool
import Control.Monad.Trans
import Control.Monad.Version

import Data.Binary
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.ClassFile.Access
import Data.ClassFile.AttributeInfo
import Data.ClassFile.CpInfo
import Data.ClassFile.Desc
import Data.ClassFile.FieldInfo
import Data.ClassFile.MethodInfo

data ClassFile = ClassFile
                 { minorVersion :: Word16
                 , majorVersion :: Word16
                 , constantPoolLength :: Word16
                 , constantPool :: [CpInfo]
                 , accessFlags :: ClassAccess
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
  put accessFlags
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

type M = ConstantPoolT Version

classM :: Word16 ->
          ClassAccess ->
          String ->
          Maybe String ->
          [String] ->
          [M FieldInfo] ->
          [M MethodInfo] ->
          [M AttributeInfo] ->
          ClassFile
classM
  version
  accessFlags
  thisClass
  superClass
  interfaces
  fields
  methods
  attributes = a
  where
    (a, constantPoolLength, constantPool) =
      (\m -> runVersion m 0 version) . runConstantPoolT $ do
        let minorVersion = 0
        majorVersion <- lift getMajorVersion
        thisClass <- lookupClass thisClass
        superClass <- maybe (return 0) lookupClass superClass
        interfaces <- mapM lookupClass interfaces
        fields <- sequence fields
        methods <- sequence methods
        attributes <- sequence attributes
        return ClassFile {..}

methodM :: ( ParameterDesc args
           , ReturnDesc result
           , MonadConstantPool m
           ) =>
           MethodAccess ->
           String ->
           args ->
           result ->
           [m AttributeInfo] ->
           m MethodInfo
methodM accessFlags name args result attributes = do
  nameIndex <- lookupUtf8 name
  descriptorIndex <- lookupUtf8 $ methodDesc args result
  attributes <- sequence attributes
  return MethodInfo {..}           

codeAttributeM :: MonadConstantPool m =>
                  Word16 ->
                  Word16 ->
                  ByteString ->
                  m AttributeInfo
codeAttributeM ms ml c = do
  attributeNameIndex <- lookupUtf8 "Code"
  let info = runPut $ do
        putWord16be ms
        putWord16be ml
        putWord32be . fromIntegral . BL.length $ c
        putLazyByteString c
        putWord16be 0
        putWord16be 0
  return AttributeInfo {..}