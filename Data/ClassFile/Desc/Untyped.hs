{-# LANGUAGE TypeSynonymInstances #-}
module Data.ClassFile.Desc.Untyped
       ( module Data.ClassFile.Desc
       , FieldType (..)
       , FieldDesc
       , V (..)
       , ParameterDesc
       , ReturnDesc ()
       ) where

import Data.ClassFile.Desc

data FieldType = B
               | C
               | D
               | F
               | I
               | J
               | L String
               | S
               | Z
               | A FieldType

type FieldDesc = FieldType

type ParameterDesc = FieldType

data V = V

instance Desc ParameterDesc where
  desc x =
    case x of
      B -> "B"
      C -> "C"
      D -> "D"
      F -> "F"
      I -> "I"
      J -> "J"
      L x -> showChar 'L' . showString x . showChar ';' $ ""
      S -> "S"
      Z -> "Z"
      A x -> showChar '[' . descs x $ ""
  
  stackSize x =
    case x of
      D -> 2
      J -> 2
      _ -> 1

instance Desc V where
  desc _ = "V"
  stackSize _ = 0

class Desc a => ReturnDesc a
instance ReturnDesc ParameterDesc
instance ReturnDesc V