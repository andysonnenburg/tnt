{-# LANGUAGE
    EmptyDataDecls
  , ExistentialQuantification
  , FlexibleInstances
  , TemplateHaskell
  , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ClassFile.Desc.Typed
       ( module Data.ClassFile.Desc
       , Int (..)
       , Long (..)
       , Float (..)
       , Double (..)
       , ReturnAddress ()
       , Reference (..)
       , Void (..)
       , FieldDesc ()
       , ParameterDesc ()
       , ReturnDesc ()
       ) where

import Control.Monad

import Data.ClassFile.Desc
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
  desc _ = "V"
  stackSize _ = 0

instance Desc () where
  descs _ = id
  stackSize _ = 0

instance FieldType a => Desc [a] where
  descs = undefined
  stackSize = foldl' ((. stackSize) . (+)) 0

class Friend a
instance Friend Int
instance Friend Long
instance Friend Float
instance Friend Double
instance Friend Reference
instance Friend ()
instance Friend Void
instance (FieldType a, Desc [a]) => Friend [a]

class Desc a => FieldType a
instance FieldType Int
instance FieldType Long
instance FieldType Float
instance FieldType Double
instance FieldType Reference

class FieldType a => ComponentType a
instance FieldType a => ComponentType a

class (Friend a, FieldType a) => FieldDesc a
instance (Friend a, FieldType a) => FieldDesc a
  
class (Friend a, Desc a) => ParameterDesc a
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
      
      friendTyp = appT (conT ''Friend) tupleTyp

    sequence [ instanceD ctxt descTyp [descsDec, stackSizeDec]
             , instanceD (cxt []) friendTyp []
             , instanceD ctxt parameterDescTyp []
             ])

class (Friend a, Desc a) => ReturnDesc a
instance ReturnDesc Int
instance ReturnDesc Long
instance ReturnDesc Float
instance ReturnDesc Double
instance ReturnDesc Reference
instance ReturnDesc Void