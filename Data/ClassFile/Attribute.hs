{-# LANGUAGE ExistentialQuantification #-}
module Data.ClassFile.Attribute
       ( Attribute (..)
       , SomeAttribute (..)
       ) where

import Control.Monad.ConstantPool.Class

import Data.ClassFile.AttributeInfo

class Attribute a where
  toAttributeInfo :: MonadConstantPool m => a -> m AttributeInfo
  
instance Attribute AttributeInfo where
  toAttributeInfo = return

data SomeAttribute = forall a. Attribute a => SomeAttribute a

instance Attribute SomeAttribute where
  toAttributeInfo (SomeAttribute x) = toAttributeInfo x