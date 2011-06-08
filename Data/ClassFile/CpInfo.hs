{-# LANGUAGE RecordWildCards #-}
module Data.ClassFile.CpInfo
       ( CpInfo (..)
       , putCpInfo
       ) where

import Data.Binary.IEEE754
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.String.UTF8
import Data.Word

data CpInfo = Class
              { nameIndex :: Word16
              }
            | Fieldref
              { classIndex :: Word16
              , nameAndTypeIndex :: Word16
              }
            | Methodref
              { classIndex :: Word16
              , nameAndTypeIndex :: Word16
              }
            | InterfaceMethodref
              { classIndex :: Word16
              , nameAndTypeIndex :: Word16
              }
            | String
              { stringIndex :: Word16
              }
            | Integer Int32
            | Float Float
            | Long Int64
            | Double Double
            | NameAndType
              { nameIndex :: Word16
              , descriptorIndex :: Word16
              }
            | Utf8
              { bytes :: String
              } deriving (Eq, Ord)

putCpInfo :: CpInfo -> Put
putCpInfo x = case x of
  Class {..} -> do
    putWord8 7
    putWord16be nameIndex
  Fieldref {..} -> do
    putWord8 9
    putWord16be classIndex
    putWord16be nameAndTypeIndex
  Methodref {..} -> do
    putWord8 10
    putWord16be classIndex
    putWord16be nameAndTypeIndex
  InterfaceMethodref {..} -> do
    putWord8 11
    putWord16be classIndex
    putWord16be nameAndTypeIndex
  String {..} -> do
    putWord8 8
    putWord16be stringIndex
  Integer y -> do
    putWord8 3
    putWord32be . fromIntegral $ y
  Float y -> do
    putWord8 4
    putFloat32be y
  Long y -> do
    putWord8 5
    putWord64be . fromIntegral $ y
  Double y -> do
    putWord8 6
    putFloat64be y
  NameAndType {..} -> do
    putWord8 12
    putWord16be nameIndex
    putWord16be descriptorIndex
  Utf8 {..} -> do
    putWord8 1
    let bytes' = toRep . fromString $ bytes
    putWord16be . fromIntegral . BL.length $ bytes'
    putLazyByteString bytes'