{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Code.Class
       ( module Data.ClassFile.Desc
       , ArrayType (..)
       , MonadCode (..)
       , ldc
       ) where

import qualified Control.Monad.Indexed as Indexed

import Data.ClassFile.Desc
import Data.Int hiding (Int)
import Data.Word

import Prelude hiding (Double, Float, Int, return)
import qualified Prelude

class ReturnAddressOrReference a
instance ReturnAddressOrReference ReturnAddress
instance ReturnAddressOrReference Reference

class CategoryF a where
  type Category a

instance CategoryF Int where
  type Category Int = One

instance CategoryF Long where
  type Category Long = Two

instance CategoryF Float where
  type Category Float = One
  
instance CategoryF Double where
  type Category Double = Two

instance CategoryF ReturnAddress where
  type Category ReturnAddress = One

instance CategoryF Reference where
  type Category Reference = One

data Zero
data Succ a

type One = Succ Zero
type Two = Succ One

class SubtractF a b where
  type Subtract a b

instance SubtractF a Zero where
  type Subtract a Zero = a

instance SubtractF (Succ a) (Succ b) where
  type Subtract (Succ a) (Succ b) = Subtract a b

class TakeF a b where
  type Take a b

instance TakeF Zero b where
  type Take Zero b = ()

instance TakeF (Succ a) (b, c) where
  type Take (Succ a) (b, c) = (b, Take (Subtract (Succ a) (Category b)) c)

class DropF a b where
  type Drop a b

instance DropF Zero b where
  type Drop Zero b = b

instance DropF (Succ a) (b, c) where
  type Drop (Succ a) (b, c) = Drop (Subtract (Succ a) (Category b)) c

class SplitAtF a b where
  type SplitAt a b

instance SplitAtF a b where
  type SplitAt a b = (Take a b, Drop a b)

class ConcatF a b where
  type Concat a b

instance ConcatF () a where
  type Concat () a = a

instance ConcatF (a, b) c where
  type Concat (a, b) c = (a, Concat b c)

class ParameterDesc a => PopF a b where
  type Pop a b

instance PopF () a where
  type Pop () a = a

instance PopF Int (Int, a) where
  type Pop Int (Int, a) = a

instance PopF Long (Long, a) where
  type Pop Long (Long, a) = a

instance PopF Float (Float, a) where
  type Pop Float (Float, a) = a
  
instance PopF Double (Double, a) where
  type Pop Double (Double, a) = a

instance PopF Reference (Reference, a) where
  type Pop Reference (Reference, a) = a
  
instance ParameterDesc (a, b) => PopF (a, b) (a, (b, c)) where
  type Pop (a, b) (a, (b, c)) = c

class ReturnDesc a => PushF a b where
  type Push a b

instance PushF Void a where
  type Push Void a = a

instance PushF Int a where
  type Push Int a = (Int, a)

instance PushF Long a where
  type Push Long a = (Long, a)

instance PushF Float a where
  type Push Float a = (Float, a)

instance PushF Double a where
  type Push Double a = (Double, a)

instance PushF Reference a where
  type Push Reference a = (Reference, a)

type Operation m i j = m i j (Label m i)

data ArrayType = T_BOOLEAN
               | T_CHAR
               | T_FLOAT
               | T_DOUBLE
               | T_BYTE
               | T_SHORT
               | T_INT
               | T_LONG

class Indexed.Monad m => MonadCode m where
  
  data Label m :: * -> *
  
  aaload :: Operation m (Int, (Reference, xs)) (Reference, xs)
  aastore :: Operation m (Reference, (Int, (Reference, xs))) xs
  aconst_null :: Operation m xs (Reference, xs)
  aload :: Word16 -> Operation m xs (Reference, xs)
  -- anewarray :: Reference -> t (Cons Int xs) (Cons Reference xs) (Label m)
  -- areturn :: m (Cons Reference xs) xs (Label m)
  -- arraylength :: m (Cons Reference xs) (Cons Int xs) (Label m)
  astore :: ReturnAddressOrReference objectref =>
            Word16 ->
	    Operation m (objectref, xs) xs
  -- athrow :: t (Cons Reference xs) xs (Label m)
  
  baload :: Operation m (Int, (Reference, xs)) (Int, xs)
  bastore :: Operation m (Int, (Int, (Reference, xs))) xs
  
  -- caload :: t (Cons Int (Cons Reference xs)) (Cons Int xs) (Label m)
  -- castore :: t (Cons Int (Cons Int (Cons Reference xs))) xs (Label m)
  -- checkcast :: Reference -> t (Cons Reference xs) (Cons Reference xs) (Label m)
  
  -- d2f :: t (Cons Double xs) (Cons Float xs) (Label m)
  -- d2i :: t (Cons Double xs) (Cons Int xs) (Label m)
  -- d2l :: t (Cons Double xs) (Cons Long xs) (Label m)
  -- dadd :: t (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  -- daload :: t (Cons Int (Cons Reference xs)) (Cons Double xs) (Label m)
  -- dastore :: t (Cons Double (Cons Int (Cons Reference xs))) xs (Label m)
  -- dcmpg :: t (Cons Double (Cons Double xs)) (Cons Int xs) (Label m)
  -- dcmpl :: t (Cons Double (Cons Double xs)) (Cons Int xs) (Label m)
  -- ddiv :: t (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  -- dload :: Word16 -> t xs (Cons Double xs) (Label m)
  -- dmul :: t (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  -- dneg :: t (Cons Double xs) (Cons Double xs) (Label m)
  -- drem :: t (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  -- dreturn :: t (Cons Double xs) xs (Label m)
  -- dstore :: Word16 -> t xs (Cons Double xs) (Label m)
  -- dsub :: t (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  dup :: Category value ~ One => Operation m (value, xs) (value, (value, xs))
  -- dup_x1 :: ( Category value1 ~ One
  --           , Category value2 ~ One
  --           ) =>
  --           m
  --           (Cons value1 (Cons value2 xs))
  --           (Cons value1 (Cons value2 (Cons value1 xs)))
  --           (Label m)
  -- dup_x2 :: ( Category value1 ~ One
  --           , (value2, xs') ~ SplitAt Two xs
  --           ) =>
  --           m
  --           (Cons value1 xs)
  --           (Cons value1 (Concat value2 (Cons value1 xs')))
  --           (Label m)
  dup2 :: (value ~ Take Two xs) => Operation m xs (Concat value xs)
  -- dup2_x1 :: ( value1 ~ Take Two xs
  --            , (value2, xs') ~ SplitAt Three xs
  --            ) => t xs (Concat value2 (Concat value1 xs')) (Label m)
  -- dup2_x2 :: ( value1 ~ Take Two xs
  --            , (value2, xs') ~ SplitAt Four xs
  --            ) => t xs (Concat value2 (Concat value1 xs')) (Label m)
             
  -- getfield :: Type value =>
  --             String ->
  --             String ->
  --             value ->
  --             t xs (Push value (Pop Reference xs)) (Label m)
  getstatic :: FieldDesc value =>
               String ->
               String ->
               value ->
               Operation m xs (Push value xs)
  
  goto :: Label m xs -> Operation m xs xs
  
  i2b :: Operation m (Int, xs) (Int, xs)
  
  iadd :: Operation m (Int, (Int, xs)) (Int, xs)
  
  iinc :: Word16 -> Int32 -> Operation m xs xs
  
  ifeq :: Label m xs -> Operation m (Int, xs) xs
  
  iload :: Word16 -> Operation m xs (Int, xs)
  
  invokeinterface :: ( ParameterDesc args  
                     , ReturnDesc result
                     ) =>
                     String ->
                     String ->
                     args ->
                     result ->
                     Operation m xs (Push result (Pop Reference (Pop args xs)))
  invokespecial :: ( ParameterDesc args
                   , ReturnDesc result
                   ) =>
                   String ->
                   String ->
                   args ->
                   result ->
                   Operation m xs (Push result (Pop Reference (Pop args xs)))
  invokevirtual :: ( ParameterDesc args
                   , ReturnDesc result
                   ) =>
                   String ->
                   String ->
                   args ->
                   result ->
                   Operation m xs (Push result (Pop Reference (Pop args xs)))
  
  istore :: Word16 -> Operation m (Int, xs) xs
  
  isub :: Operation m (Int, (Int, xs)) (Int, xs)
  
  ldcInt :: Int32 -> Operation m xs (Int, xs)
  ldcFloat :: Prelude.Float -> Operation m xs (Float, xs)
  ldcString :: String -> Operation m xs (Reference, xs)
  ldcClass :: String -> Operation m xs (Reference, xs)
  ldcLong :: Int64 -> Operation m xs (Long, xs)
  ldcDouble :: Prelude.Double -> Operation m xs (Double, xs)
  
  new :: String -> Operation m xs (Reference, xs)
  newarray :: ArrayType -> Operation m (Int, xs) (Reference, xs)
  nop :: Operation m xs xs
  
  return :: Operation m xs xs
  
class MonadCode m => LDC a b m | a -> b where
  ldc :: b -> Operation m xs (a, xs)

instance MonadCode m => LDC Int Int32 m where
  ldc = ldcInt

instance MonadCode m => LDC Float Prelude.Float m where
  ldc = ldcFloat

instance MonadCode m => LDC Reference String m where
  ldc = ldcString

instance MonadCode m => LDC Long Int64 m where
  ldc = ldcLong

instance MonadCode m => LDC Double Prelude.Double m where
  ldc = ldcDouble