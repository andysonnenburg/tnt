{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.ClassFile.Desc
       ( Int (..)
       , Long (..)
       , Float (..)
       , Double (..)
       , ReturnAddress ()
       , Reference (..)
       , Void (..)
       , FieldDesc ()
       , ParameterDesc ()
       , ReturnDesc ()
       , desc
       , descs
       , stackSize
       , methodDesc
       , internalName
       ) where

import Control.Monad

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

class Desc a => FieldType a
instance FieldType Int
instance FieldType Long
instance FieldType Float
instance FieldType Double
instance FieldType Reference

class FieldType a => ComponentType a
instance FieldType a => ComponentType a

class FieldType a => FieldDesc a
instance FieldType a => FieldDesc a
  
class Desc a => ParameterDesc a
instance ParameterDesc Int
instance ParameterDesc Long
instance ParameterDesc Float
instance ParameterDesc Double
instance ParameterDesc Reference
instance ParameterDesc ()

$(do
  let n = min 255 maxTupleSize
  names <- replicateM n $ newName "a"
  liftM concat $ forM [2 .. n] $ \i -> do
  
    let names' = take i names
        tvs = map varT names'
        ctxt = cxt . map (\tv -> classP ''FieldType [tv]) $ tvs
        tupleTyp = foldl' appT (tupleT i) tvs
  
    let
      p = tupP . map varP $ names'
      es = map varE names'

      descTyp = appT (conT ''Desc) tupleTyp

      descsDec = funD 'descs [clause [p] (normalB g) []]
        where
          xs = map (\e -> [| descs $e |]) es
          g = foldr1 (\a b -> [| $a . $b |]) xs

      stackSizeDec = funD 'stackSize [clause [p] (normalB g) []]
        where
          xs = map (\e -> [| stackSize $e |]) es
          g = foldl1' (\a b -> [| ($a :: Word16) + $b |]) xs

      parameterDescTyp = appT (conT ''ParameterDesc) tupleTyp

    sequence [ instanceD ctxt descTyp [descsDec, stackSizeDec]
             , instanceD ctxt parameterDescTyp []
             ])

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