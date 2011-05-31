{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Code.Class
       ( MonadCode (..)
       , ldc
       , ArrayType (..)
       , Int (..)
       , Long (..)
       , Float (..)
       , Double (..)
       , ReturnAddress ()
       , Reference (..)
       , Void (..)
       , ParameterDesc ()
       , ReturnDesc ()
       , desc
       , descs
       , stackSize
       , methodDesc
       , internalName
       ) where

import Control.Monad (forM, replicateM)
import qualified Control.Monad.Parameterized as Parameterized

import Data.Int hiding (Int)
import Data.List
import Data.Word

import GHC.Exts (maxTupleSize)

import Language.Haskell.TH

import Prelude hiding (Double, Float, Int, return)
import qualified Prelude

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

instance CategoryF Reference where
  type Category Reference = One

data Zero
data Succ a

class SubtractF a b where
  type Subtract a b

instance SubtractF a Zero where
  type Subtract a Zero = a

instance SubtractF (Succ a) (Succ b) where
  type Subtract (Succ a) (Succ b) = Subtract a b

type One = Succ Zero
type Two = Succ One
type Three = Succ Two

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

class Parameterized.Monad m => MonadCode m where
  
  data Label m :: * -> *
  
  aaload :: Operation m (Int, (Reference, xs)) (Reference, xs)
  aastore :: Operation m (Reference, (Int, (Reference, xs))) xs
  aconst_null :: Operation m xs (Reference, xs)
  aload :: Word16 -> Operation m xs (Reference, xs)
  -- anewarray :: Reference -> m (Cons Int xs) (Cons Reference xs) (Label m)
  -- areturn :: m (Cons Reference xs) xs (Label m)
  -- arraylength :: m (Cons Reference xs) (Cons Int xs) (Label m)
  astore :: Word16 -> Operation m (Reference, xs) xs
  -- athrow :: m (Cons Reference xs) xs (Label m)
  
  baload :: Operation m (Int, (Reference, xs)) (Int, xs)
  bastore :: Operation m (Int, (Int, (Reference, xs))) xs
  
  -- caload :: m (Cons Int (Cons Reference xs)) (Cons Int xs) (Label m)
  -- castore :: m (Cons Int (Cons Int (Cons Reference xs))) xs (Label m)
  -- checkcast :: Reference -> m (Cons Reference xs) (Cons Reference xs) (Label m)
  
  -- d2f :: m (Cons Double xs) (Cons Float xs) (Label m)
  -- d2i :: m (Cons Double xs) (Cons Int xs) (Label m)
  -- d2l :: m (Cons Double xs) (Cons Long xs) (Label m)
  -- dadd :: m (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  -- daload :: m (Cons Int (Cons Reference xs)) (Cons Double xs) (Label m)
  -- dastore :: m (Cons Double (Cons Int (Cons Reference xs))) xs (Label m)
  -- dcmpg :: m (Cons Double (Cons Double xs)) (Cons Int xs) (Label m)
  -- dcmpl :: m (Cons Double (Cons Double xs)) (Cons Int xs) (Label m)
  -- ddiv :: m (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  -- dload :: Word16 -> m xs (Cons Double xs) (Label m)
  -- dmul :: m (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  -- dneg :: m (Cons Double xs) (Cons Double xs) (Label m)
  -- drem :: m (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
  -- dreturn :: m (Cons Double xs) xs (Label m)
  -- dstore :: Word16 -> m xs (Cons Double xs) (Label m)
  -- dsub :: m (Cons Double (Cons Double xs)) (Cons Double xs) (Label m)
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
  --            ) => m xs (Concat value2 (Concat value1 xs')) (Label m)
  -- dup2_x2 :: ( value1 ~ Take Two xs
  --            , (value2, xs') ~ SplitAt Four xs
  --            ) => m xs (Concat value2 (Concat value1 xs')) (Label m)
             
  -- getfield :: Type value =>
  --             String ->
  --             String ->
  --             value ->
  --             m xs (Push value (Pop Reference xs)) (Label m)
  getstatic :: FieldType value =>
               String ->
               String ->
               value ->
               Operation m xs (Push value xs)
  
  goto :: Label m xs -> Operation m xs xs
  
  i2b :: Operation m (Int, xs) (Int, xs)
  
  iadd :: Operation m (Int, (Int, xs)) (Int, xs)
  
  iinc :: Word16 -> Int16 -> Operation m xs xs
  
  ifeq :: Label m xs -> Operation m (Int, xs) xs
  
  iload :: Word16 -> Operation m xs (Int, xs)
  
  invokeinterface :: ( ParameterDesc args  
                     , ReturnDesc result
                     ) =>
                     String ->
                     String ->
                     args ->
                     result ->
                     Operation m xs
                     (Push result (Pop Reference (Pop args xs)))
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
                   Reference ->
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