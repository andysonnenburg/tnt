{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.ClassFile.Desc
       ( ArrayType (..)
       , Int (..)
       , Long (..)
       , Float (..)
       , Double (..)
       , ReturnAddress ()
       , Reference (..)
       , Void (..)
       , FieldType ()
       , ParameterDesc ()
       , ReturnDesc ()
       , desc
       , descs
       , stackSize
       , methodDesc
       , internalName
       ) where

import Control.Monad (forM, replicateM)

import Data.List
import Data.Word

import GHC.Exts (maxTupleSize)

import Language.Haskell.TH

import Prelude hiding (Double, Float, Int)

data Int = B | S | I | C | Z
data Long = J

data Float = F
data Double = D

data ReturnAddress

data Reference = L String
               | forall a. ComponentType a => A a

data Void = V

class Desc a where
  descs :: a -> ShowS
  desc :: a -> String
  stackSize :: a -> Word16
  
  desc = flip descs ""
  descs = (++) . desc
  stackSize _ = 1

instance Desc Int where
  desc B = "B"
  desc S = "S"
  desc I = "I"
  desc C = "C"
  desc Z = "Z"

instance Desc Long where
  desc = const "L"
  stackSize _ = 2

instance Desc Float where
  desc = const "F"

instance Desc Double where
  desc = const "D"
  stackSize _ = 2

instance Desc Reference where
  descs (L x) = showChar 'L' . showString x . showChar ';'
  descs (A x) = showChar '[' . descs x

instance Desc Void where
  desc = const "V"
  stackSize _ = 0

instance Desc () where
  descs _ = id
  stackSize _ = 0

class Desc a => FieldType a where
instance FieldType Int where
instance FieldType Long where
instance FieldType Float where
instance FieldType Double where
instance FieldType Reference where

class FieldType a => ComponentType a where
instance FieldType a => ComponentType a where
  
class Desc a => ParameterDesc a where
instance ParameterDesc Int where
instance ParameterDesc Long where
instance ParameterDesc Float where
instance ParameterDesc Double where
instance ParameterDesc Reference where
instance ParameterDesc () where

$(forM [2 :: Word8 .. fromIntegral maxTupleSize] $ \i -> do
  let i' = fromIntegral i
  
  names <- replicateM i' (newName "a")
  
  let tvs = map varT names
      ctxt = cxt . map (classP ''FieldType . (:[])) $ tvs
      typ = appT (conT ''Desc) . foldl' appT (tupleT i') $ tvs
  
  let
    p = tupP . map varP $ names
    es = map varE names
    
    descsDec =
      let
        xs = map (\e -> [| descs $e |]) es
        g = foldr1 (\a b -> [| $a . $b |]) xs
      in funD 'descs [clause [p] (normalB g) []]
     
    stackSizeDec =
      let
        xs = map (\e -> [| stackSize $e |]) es
        g = foldl1' (\a b -> [| ($a :: Word16) + $b |]) xs
      in funD 'stackSize [clause [p] (normalB g) []]

  instanceD ctxt typ [descsDec, stackSizeDec])

  
$(forM [2 :: Word8 .. fromIntegral maxTupleSize] $ \i -> do
  let i' = fromIntegral i
      
  names <- replicateM i' (newName "a")
  
  let tvs = map varT names
      ctxt = cxt . map (\tv -> classP ''FieldType [tv]) $ tvs
      typ = appT (conT ''ParameterDesc) . foldl' appT (tupleT i') $ tvs

  instanceD ctxt typ [])

class Desc a => ReturnDesc a where
instance ReturnDesc Int where
instance ReturnDesc Long where
instance ReturnDesc Float where
instance ReturnDesc Double where
instance ReturnDesc Reference where
instance ReturnDesc Void where

methodDesc :: (ParameterDesc a, ReturnDesc b) => a -> b -> String
methodDesc a b = (showChar '(' . descs a . showChar ')' . descs b) ""
  
internalName :: Reference -> String
internalName (L x) = x  
internalName (A x) = (showChar '[' . descs x) ""

data ArrayType = T_BOOLEAN
               | T_CHAR
               | T_FLOAT
               | T_DOUBLE
               | T_BYTE
               | T_SHORT
               | T_INT
               | T_LONG