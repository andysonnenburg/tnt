{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , TypeFamilies
  , TypeSynonymInstances #-}
module Control.Monad.Code.Class.Untyped
       ( MonadCode (..)
       , ldc
       ) where

import Data.ClassFile.Desc.Untyped
import Data.Int
import Data.Word

class MonadCode m where
  
  type Label m
  
  type ArrayType m
  
  boolean :: ArrayType m
  char :: ArrayType m
  float :: ArrayType m
  double :: ArrayType m
  byte :: ArrayType m
  short :: ArrayType m
  int :: ArrayType m
  long :: ArrayType m
  
  aaload :: m (Label m)
  aastore :: m (Label m)
  aconst_null :: m (Label m)
  aload :: Word16 -> m (Label m)
  anewarray :: String -> m (Label m)
  areturn :: m (Label m)
  arraylength :: m (Label m)
  astore :: Word16 -> m (Label m)
  athrow :: m (Label m)
  
  baload :: m (Label m)
  bastore :: m (Label m)
  checkcast :: String -> m (Label m)
  
  dup :: m (Label m)
  dup_x1 :: m (Label m)
  dup_x2 :: m (Label m)
  dup2 :: m (Label m)
  dup2_x1 :: m (Label m)
  dup2_x2 :: m (Label m)
             
  getfield :: String -> String -> FieldDesc -> m (Label m)
  getstatic :: String -> String -> FieldDesc -> m (Label m)
  
  goto :: Label m -> m (Label m)
  
  i2b :: m (Label m)
  
  iadd :: m (Label m)
  
  iinc :: Word16 -> Int32 -> m (Label m)
  
  ifeq :: Label m -> m (Label m)
  ifne :: Label m -> m (Label m)
  
  iload :: Word16 -> m (Label m)
  
  invokeinterface :: ReturnDesc return =>
                     String ->
                     String ->
                     [ParameterDesc] ->
                     return ->
                     m (Label m)
  invokespecial :: ReturnDesc return =>
                   String ->
                   String ->
                   [ParameterDesc] ->
                   return ->
                   m (Label m)
  invokestatic :: ReturnDesc return =>
                  String ->
                  String ->
                  parameter ->
                  return -> 
                  m (Label m)
  invokevirtual :: ReturnDesc return =>
                   String ->
                   String ->
                   [ParameterDesc] ->
                   return ->
                   m (Label m)
  
  istore :: Word16 -> m (Label m)
  
  isub :: m (Label m)
  
  ldcInt :: Int32 -> m (Label m)
  ldcFloat :: Prelude.Float -> m (Label m)
  ldcString :: String -> m (Label m)
  ldcClass :: String -> m (Label m)
  ldcLong :: Int64 -> m (Label m)
  ldcDouble :: Prelude.Double -> m (Label m)
  
  new :: String -> m (Label m)
  newarray :: ArrayType m -> m (Label m)
  nop :: m (Label m)
  
  pop :: m (Label m)

  return :: m (Label m)
  
  swap :: m (Label m)

class MonadCode m => Ldc a m where
  ldc :: a -> m (Label m)

instance MonadCode m => Ldc Int32 m where
  ldc = ldcInt

instance MonadCode m => Ldc Prelude.Float m where
  ldc = ldcFloat

instance MonadCode m => Ldc String m where
  ldc = ldcString

instance MonadCode m => Ldc Int64 m where
  ldc = ldcLong

instance MonadCode m => Ldc Prelude.Double m where
  ldc = ldcDouble