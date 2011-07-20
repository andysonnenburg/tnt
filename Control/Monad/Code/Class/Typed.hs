{-# LANGUAGE
    EmptyDataDecls 
  , FlexibleContexts 
  , FlexibleInstances 
  , FunctionalDependencies 
  , MultiParamTypeClasses 
  , TypeFamilies 
  , TypeSynonymInstances 
  , UndecidableInstances #-}
module Control.Monad.Code.Class.Typed
       ( module Data.ClassFile.Desc.Typed
       , MonadCode (..)
       , ldc
       ) where

import qualified Control.Monad.Indexed as Indexed

import Data.ClassFile.Desc.Typed
import Data.Int hiding (Int)
import Data.Word

import Prelude hiding (Double, Float, Int, return)
import qualified Prelude

data Z
data S a

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three

class Category a b | a -> b
instance Category Int One
instance Category Long Two
instance Category Float One
instance Category Double Two
instance Category ReturnAddress One
instance Category Reference One

class Add2 a b c | a b -> c, c a -> b
instance Add2 Z a a
instance Add2 a b c => Add2 (S a) b (S c)

class Add a b c | a b -> c, b c -> a, c a -> b
instance (Add2 a b c, Add2 b a c) => Add a b c

class Subtract2 a b c | a b -> c, b c -> a
instance Subtract2 a Z a
instance Subtract2 a b c => Subtract2 (S a) (S b) c

class Subtract a b c | a b -> c, b c -> a, c a -> b
instance (Subtract2 a b c, Add b c a) => Subtract a b c

class Take a b c | a b -> c
instance Take Z xs ()
instance ( Category x cat
         , Subtract (S n) cat n'
         , Take n' xs ys
         ) => Take (S n) (x, xs) (x, ys)

class Concat a b c | a b -> c, c a -> b
instance Concat () xs xs
instance Concat xs ys xs' => Concat (x, xs) ys (x, xs')

class Drop a b c | a b -> c
instance Drop Z xs xs
instance ( Category x cat
         , Subtract (S n) cat n'
         , Drop n' xs ys
         ) => Drop (S n) (x, xs) ys

class ParameterDesc a => Pop a b c | a b -> c
instance Pop () a a
instance Pop Int (Int, a) a
instance Pop Long (Long, a) a
instance Pop Float (Float, a) a
instance Pop Double (Double, a) a
instance Pop Reference (Reference, a) a
instance ParameterDesc (a, b) => Pop (a, b) (a, (b, c)) c

class ReturnDesc a => Push a b c | a b -> c, b c -> a, c a -> b
instance Push Int a (Int, a)
instance Push Long a (Long, a)
instance Push Float a (Float, a)
instance Push Double a (Double, a)
instance Push Reference a (Reference, a)
instance Push Void a a

class ReturnAddressOrReference a
instance ReturnAddressOrReference ReturnAddress
instance ReturnAddressOrReference Reference

type Operation m p q = m p q (Label m p)

class Indexed.Monad m => MonadCode m where
  
  data ArrayType m
  
  type Label m :: * -> *
  
  boolean :: ArrayType m
  char :: ArrayType m
  float :: ArrayType m
  double :: ArrayType m
  byte :: ArrayType m
  short :: ArrayType m
  int :: ArrayType m
  long :: ArrayType m
  
  aaload :: Operation m (Int, (Reference, xs)) (Reference, xs)
  aastore :: Operation m (Reference, (Int, (Reference, xs))) xs
  aconst_null :: Operation m xs (Reference, xs)
  aload :: Word16 -> Operation m xs (Reference, xs)
  anewarray :: String -> Operation m (Int, xs) (Reference, xs)
  areturn :: Operation m (Reference, xs) xs
  arraylength :: Operation m (Reference, xs) (Int, xs)
  astore :: ReturnAddressOrReference x =>
            Word16 ->
            Operation m (x, xs) xs
  athrow :: Operation m (Reference, xs) xs
  
  baload :: Operation m (Int, (Reference, xs)) (Int, xs)
  bastore :: Operation m (Int, (Int, (Reference, xs))) xs
  
  checkcast :: String -> Operation m (Reference, xs) (Reference, xs)
  
  dup :: ( Take One xs x
         , Concat x xs ys
         ) => Operation m xs ys
  dup_x1 :: ( Take One xs x
            , Take Two xs y
            , Drop Two xs xs'
            , Concat x xs' xs''
            , Concat y xs'' ys
            ) => Operation m xs ys
  dup_x2 :: ( Take One xs x
            , Take Three xs y
            , Drop Three xs xs'
            , Concat x xs' xs''
            , Concat y xs'' ys
            ) => Operation m xs ys
  dup2 :: ( Take Two xs x
          , Concat x xs ys
          ) => Operation m xs ys
  dup2_x1 :: ( Take Two xs x
             , Take Three xs y
             , Drop Three xs xs'
             , Concat x xs' xs''
             , Concat y xs'' ys
             ) => Operation m xs ys
  dup2_x2 :: ( Take Two xs x
             , Take Four xs y
             , Drop Four xs xs'
             , Concat x xs' xs''
             , Concat y xs'' ys
             ) => Operation m xs ys
             
  getfield :: ( FieldDesc x
              , Pop Reference xs xs'
              , Push x xs' ys
              ) =>
              String ->
              String ->
              x ->
              Operation m xs ys
  getstatic :: ( FieldDesc x
               , Push x xs ys
               ) =>
               String ->
               String ->
               x ->
               Operation m xs ys
  
  goto :: Label m xs -> Operation m xs xs
  
  i2b :: Operation m (Int, xs) (Int, xs)
  
  iadd :: Operation m (Int, (Int, xs)) (Int, xs)
  
  iinc :: Word16 -> Int32 -> Operation m xs xs
  
  ifeq :: Label m xs -> Operation m (Int, xs) xs
  ifne :: Label m xs -> Operation m (Int, xs) xs
  
  iload :: Word16 -> Operation m xs (Int, xs)
  
  invokeinterface :: ( ParameterDesc parameters  
                     , ReturnDesc return
                     , Pop parameters xs xs'
                     , Pop Reference xs' xs''
                     , Push return xs'' ys
                     ) =>
                     String ->
                     String ->
                     parameters ->
                     return ->
                     Operation m xs ys
  invokespecial :: ( ParameterDesc parameters
                   , ReturnDesc return
                   , Pop parameters xs xs'
                   , Pop Reference xs' xs''
                   , Push return xs'' ys
                   ) =>
                   String ->
                   String ->
                   parameters ->
                   return ->
                   Operation m xs ys
  invokestatic :: ( ParameterDesc parameters
                  , ReturnDesc return
                  , Pop parameters xs xs'
                  , Push return xs' ys
                  ) =>
                  String ->
                  String ->
                  parameters ->
                  return -> 
                  Operation m xs ys
  invokevirtual :: ( ParameterDesc parameters
                   , ReturnDesc return
                   , Pop parameters xs xs'
                   , Pop Reference xs' xs''
                   , Push return xs'' ys
                   ) =>
                   String ->
                   String ->
                   parameters ->
                   return ->
                   Operation m xs ys
  
  istore :: Word16 -> Operation m (Int, xs) xs
  
  isub :: Operation m (Int, (Int, xs)) (Int, xs)
  
  ldcInt :: Int32 -> Operation m xs (Int, xs)
  ldcFloat :: Prelude.Float -> Operation m xs (Float, xs)
  ldcString :: String -> Operation m xs (Reference, xs)
  ldcClass :: String -> Operation m xs (Reference, xs)
  ldcLong :: Int64 -> Operation m xs (Long, xs)
  ldcDouble :: Prelude.Double -> Operation m xs (Double, xs)
  
  new :: String -> Operation m xs (Reference, xs)
  newarray :: ArrayType m -> Operation m (Int, xs) (Reference, xs)
  nop :: Operation m xs xs
  
  pop :: Category x One => Operation m (x, xs) xs

  return :: Operation m xs xs
  
  swap :: ( Category x One
          , Category y One
          ) => Operation m (x, (y, xs)) (y, (x, xs))
  
class MonadCode m => Ldc a b m | a -> b, b -> a where
  ldc :: b -> Operation m xs (a, xs)

instance MonadCode m => Ldc Int Int32 m where
  ldc = ldcInt

instance MonadCode m => Ldc Float Prelude.Float m where
  ldc = ldcFloat

instance MonadCode m => Ldc Reference String m where
  ldc = ldcString

instance MonadCode m => Ldc Long Int64 m where
  ldc = ldcLong

instance MonadCode m => Ldc Double Prelude.Double m where
  ldc = ldcDouble
