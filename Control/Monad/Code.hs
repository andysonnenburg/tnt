{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Code
       ( module Control.Monad.Code.Class
       , Control.Monad.Code.Class.Int (..)
       , Control.Monad.Code.Class.return
       , Code
       , runCode
       , execCode
       , CodeT
       , runCodeT
       , execCodeT
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Code.Class hiding (Int (..), return)
import qualified Control.Monad.Code.Class
import qualified Control.Monad.Code.Opcode as Opcode
import Control.Monad.ConstantPool
import Control.Monad.Fix
import Control.Monad.Indexed.Class hiding (Monad)
import qualified Control.Monad.Indexed.Class as Indexed
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as State
import Control.Monad.Version

import Data.Binary.Put
import Data.ClassFile
import Data.ClassFile.Access
import Data.ClassFile.MethodInfo
import Data.Int
import Data.Word

import Prelude hiding (Double, Float, Int, return)

type Code s = CodeT s (ConstantPoolT Version)

runCode :: forall parameters result i a.
           ( ParameterDesc parameters
           , ReturnDesc result
           ) =>
           MethodAccess ->
           String ->
           parameters ->
           result ->
           (forall s. Code s () i a) ->
           ConstantPoolT Version (a, MethodInfo)
runCode = runCodeT

execCode :: forall parameters result i a.
            ( ParameterDesc parameters
            , ReturnDesc result
            ) =>
            MethodAccess ->
            String ->
            parameters ->
            result ->
            (forall s. Code s () i a) ->
            ConstantPoolT Version MethodInfo
execCode = execCodeT

data S = S { stack :: {-# UNPACK #-} !Word16
           , maxStack :: {-# UNPACK #-} !Word16
           , maxLocals :: {-# UNPACK #-} !Word16
           , codeLength :: {-# UNPACK #-} !Int32
           , code :: {-# UNPACK #-} !Put
           }

data PairS a = a :+: S

newtype StateT m a = StateT { runStateT :: S -> m (PairS a) }

instance Monad m => Monad (StateT m) where
  return a = StateT $ \s -> s `seq` return (a :+: s)
  {-# INLINE return #-}
  
  m >>= k = StateT $ \s -> do
    ~(a :+: s') <- runStateT m s
    runStateT (k a) s'
  {-# INLINE (>>=) #-}

  fail str = StateT $ \_ -> fail str

instance MonadFix m => MonadFix (StateT m) where
  mfix f = StateT $ \s -> s `seq` mfix $ \ ~(a :+: _) -> runStateT (f a) s

lift :: Monad m => m a -> StateT m a
lift m = StateT $ \s -> s `seq` do
  a <- m
  return (a :+: s)
{-# INLINE lift #-}

get :: Monad m => StateT m S
get = StateT $ \s -> s `seq` return (s :+: s)
{-# INLINE get #-}

put :: Monad m => S -> StateT m ()
put s = s `seq` StateT $ \_ -> return (() :+: s)
{-# INLINE put #-}

newtype CodeT s m i j a = CodeT
                          { unCodeT :: StateT m a
                          } deriving ( Monad
                                     , MonadFix
                                     )

instance MonadState s m => MonadState s (CodeT s' m i j) where
  get = CodeT . lift $ State.get
  put = CodeT . lift . State.put

instance Monad m => Indexed.Monad (CodeT s m) where
  ireturn = CodeT . return
  {-# INLINE ireturn #-}
  
  m `ibind` k = CodeT $ unCodeT m >>= unCodeT . k
  {-# INLINE ibind #-}
  
  ifail = CodeT . fail

runCodeT :: forall parameters result m i a.
            ( ParameterDesc parameters
            , ReturnDesc result
            , MonadConstantPool m
            ) =>
            MethodAccess ->
            String ->
            parameters ->
            result ->
            (forall s. CodeT s m () i a) ->
            m (a, MethodInfo)
runCodeT access name args result (CodeT m) = do
  a :+: S _ ms ml _ c <- runStateT m initState
  let codeAttribute = codeAttributeM ms (ml + stackSize args) (runPut c)
  method <- methodM access name args result [codeAttribute]
  return (a, method)

execCodeT :: forall parameters result m i a.
             ( ParameterDesc parameters
             , ReturnDesc result
             , MonadConstantPool m
             ) =>
             MethodAccess ->
             String ->
             parameters ->
             result ->
             (forall s. CodeT s m () i a) ->
             m MethodInfo
execCodeT access name parameters result =
  liftM snd . runCodeT access name parameters result

initState :: S
initState = S { stack = 0
              , maxStack = 0
              , maxLocals = 0
              , codeLength = 0
              , code = return ()
              }

instance (MonadFix m, MonadConstantPool m) => MonadCode (CodeT s m) where
  
  newtype Label (CodeT s m) xs = Label Int32

  newtype ArrayType (CodeT s m) = ArrayType { unArrayType :: Word8 }

  boolean = ArrayType 4
  char = ArrayType 5
  float = ArrayType 6
  double = ArrayType 7
  byte = ArrayType 8
  short = ArrayType 9
  int = ArrayType 10
  long = ArrayType 11

  aaload = insn (-1) Opcode.aaload
  aastore = insn (-3) Opcode.aastore
  aconst_null = insn 1 Opcode.aconst_null

  aload var = case var of
    0 -> insn' 1 1 1 $ putWord8 Opcode.aload_0
    1 -> insn' 1 2 1 $ putWord8 Opcode.aload_1
    2 -> insn' 1 3 1 $ putWord8 Opcode.aload_2
    3 -> insn' 1 4 1 $ putWord8 Opcode.aload_3
    _ -> varInsn 1 var Opcode.aload

  astore var = case var of
    0 -> insn' (-1) 1 1 $ putWord8 Opcode.astore_0
    1 -> insn' (-1) 2 1 $ putWord8 Opcode.astore_1
    2 -> insn' (-1) 3 1 $ putWord8 Opcode.astore_2
    3 -> insn' (-1) 4 1 $ putWord8 Opcode.astore_3
    _ -> varInsn (-1) var Opcode.astore

  baload = insn (-1) Opcode.baload

  bastore = insn (-3) Opcode.bastore

  dup = insn 1 Opcode.dup

  dup2 = insn 2 Opcode.dup2

  getstatic typ name fieldType = CodeT $ do
    x <- lift $ lookupField typ name (desc fieldType)
    unCodeT $ insn' (stackSize fieldType) 0 3 $ do
      putWord8 Opcode.getstatic
      putWord16be x

  goto (Label target) = CodeT $ do
    s@S {..} <- get
    let offset = target - codeLength
    when (offset > fromIntegral (maxBound :: Int16) ||
          offset < fromIntegral (minBound :: Int16)) $
      fail "TODO: wide branch offsets"
    put s { codeLength = codeLength + 3
          , code = code >> do
            putWord8 Opcode.goto
            putWord16be . fromIntegral $ offset
          }
    return . Label $ codeLength

  i2b = insn 0 Opcode.i2b
  
  iadd = insn (-1) Opcode.iadd

  ifeq (Label target) = CodeT $ do
    s@S {..} <- get
    let offset = target - codeLength
    when (offset > fromIntegral (maxBound :: Int16) ||
          offset < fromIntegral (minBound :: Int16)) $
      fail "TODO: wide branch offsets"                                          
    put s { stack = stack - 1
          , codeLength = codeLength + 3
          , code = code >> do
            putWord8 Opcode.ifeq
            putWord16be . fromIntegral $ offset
          }
    return . Label $ codeLength
  
  ifne (Label target) = CodeT $ do
    s@S {..} <- get
    let offset = target - codeLength
    when (offset > fromIntegral (maxBound :: Int16) ||
          offset < fromIntegral (minBound :: Int16)) $
      fail "TODO: wide branch offsets"
    put s { stack = stack - 1
          , codeLength = codeLength + 3
          , code = code >> do
            putWord8 Opcode.ifne
            putWord16be . fromIntegral $ offset
          }
    return . Label $ codeLength
  
  iinc local cnst
    | local <= fromIntegral (maxBound :: Word8) &&
      cnst >= fromIntegral (minBound :: Int8) &&
      cnst <= fromIntegral (maxBound :: Int8) =
      insn' 0 local 3 $ do
        putWord8 Opcode.iinc
        putWord8 . fromIntegral $ local
        putWord8 . fromIntegral $ cnst
    | cnst >= fromIntegral (minBound :: Int16) &&
      cnst <= fromIntegral (maxBound :: Int16) =
      insn' 0 local 6 $ do
        putWord8 Opcode.wide
        putWord8 Opcode.iinc
        putWord16be local
        putWord16be . fromIntegral $ cnst
    | otherwise = iload local `ibind` \label ->
                  ldc cnst `ithen`
                  iadd `ithen`
                  istore local `ithen`
                  ireturn label

  iload j = case j of
    0 -> insn' 1 1 1 $ putWord8 Opcode.iload_0
    1 -> insn' 1 2 1 $ putWord8 Opcode.iload_1
    2 -> insn' 1 3 1 $ putWord8 Opcode.iload_2
    3 -> insn' 1 4 1 $ putWord8 Opcode.iload_3
    _ -> varInsn 1 j Opcode.iload

  istore var = case var of
    0 -> insn' (-1) 1 1 $ putWord8 Opcode.istore_0
    1 -> insn' (-1) 2 1 $ putWord8 Opcode.istore_1
    2 -> insn' (-1) 3 1 $ putWord8 Opcode.istore_2
    3 -> insn' (-1) 4 1 $ putWord8 Opcode.istore_3
    _ -> varInsn (-1) var Opcode.istore

  invokeinterface typ method args result = CodeT $ do
    x <- lift $ lookupInterfaceMethod typ method dsc
    unCodeT $ insn' i 0 5 $ do
      putWord8 Opcode.invokeinterface
      putWord16be x
      putWord8 . (+ 1) . fromIntegral . stackSize $ args
      putWord8 0
    where
      dsc = methodDesc args result
      i = stackSize result - stackSize args

  invokespecial typ method args result = CodeT $ do
    x <- lift $ lookupMethod typ method dsc
    unCodeT $ insn' i 0 3 $ do
      putWord8 Opcode.invokespecial
      putWord16be x
    where
      dsc = methodDesc args result
      i = stackSize result - stackSize args

  invokevirtual typ method args result = CodeT $ do
    x <- lift $ lookupMethod typ method dsc
    unCodeT $ insn' i 0 3 $ do
      putWord8 Opcode.invokevirtual
      putWord16be x
    where
      dsc = methodDesc args result
      i = stackSize result - stackSize args

  isub = insn (-1) Opcode.isub

  ldcInt x = case x of
    (-1) -> insn 1 Opcode.iconst_m1
    0 -> insn 1 Opcode.iconst_0
    1 -> insn 1 Opcode.iconst_1
    2 -> insn 1 Opcode.iconst_2
    3 -> insn 1 Opcode.iconst_3
    4 -> insn 1 Opcode.iconst_4
    5 -> insn 1 Opcode.iconst_5
    _
      | x >= fromIntegral (minBound :: Int8) &&
        x <= fromIntegral (maxBound :: Int8) -> insn' 1 0 2 $ do
          putWord8 Opcode.bipush
          putWord8 . fromIntegral $ x
      | x >= fromIntegral (minBound :: Int16) &&
        x <= fromIntegral (maxBound :: Int16) -> insn' 1 0 3 $ do
          putWord8 Opcode.sipush
          putWord16be . fromIntegral $ x
      | otherwise -> ldcInsn lookupInteger x

  ldcFloat x = case x of
    0 -> insn 1 Opcode.fconst_0
    1 -> insn 1 Opcode.fconst_1
    2 -> insn 1 Opcode.fconst_2
    _ -> ldcInsn lookupFloat x

  ldcString = ldcInsn lookupString

  ldcClass = ldcInsn lookupClass

  ldcLong x = case x of
    0 -> insn 2 Opcode.lconst_0
    1 -> insn 2 Opcode.lconst_1
    _ -> ldc2Insn lookupLong x

  ldcDouble x = case x of
    0 -> insn 2 Opcode.dconst_0
    1 -> insn 2 Opcode.dconst_1
    _ -> ldc2Insn lookupDouble x

  new typ = CodeT $ do
    x <- lift $ lookupClass typ
    unCodeT $ insn' 1 0 3 $ do
      putWord8 Opcode.new
      putWord16be x

  newarray typ = insn' 0 0 2 $ do
    putWord8 Opcode.newarray
    putWord8 . unArrayType $ typ

  nop = CodeT $ liftM (Label . codeLength) get

  return = insn 0 Opcode.return

insn' :: Monad m =>
         Word16 ->
         Word16 ->
         Int32 ->
         Put ->
         CodeT s m i j (Label (CodeT s m) i)
insn' i j n m = CodeT $ do
  S {..} <- get
  let stack' = stack + i
  put S { stack = stack'
        , maxStack = max maxStack stack'
        , maxLocals = max maxLocals j
        , codeLength = codeLength + n
        , code = code >> m
        }
  return . Label $ codeLength

insn :: Monad m => Word16 -> Word8 -> CodeT s m i j (Label (CodeT s m) i)
insn i opcode = insn' i 0 1 $ putWord8 opcode

varInsn :: Monad m =>
           Word16 ->
           Word16 ->
           Word8 ->
           CodeT s m i j (Label (CodeT s m) i)
varInsn i var opcode
  | var < fromIntegral (maxBound :: Word8) = insn' i (var + 1) 2 $ do
    putWord8 opcode
    putWord8 . fromIntegral $ var
  | otherwise = insn' i (var + 1) 3 $ do
    putWord8 Opcode.wide
    putWord8 opcode
    putWord16be var

ldcInsn :: MonadConstantPool m =>
           (a -> m Word16) ->
           a ->
           CodeT s m i j (Label (CodeT s m) i)
ldcInsn k x = CodeT $ do
  x' <- lift $ k x
  let (n, p, o) = if x' < fromIntegral (maxBound :: Word8)
                  then (2, putWord8 . fromIntegral, Opcode.ldc)
                  else (3, putWord16be, Opcode.ldc_w)
  unCodeT $ insn' 1 0 n $ do
    putWord8 o
    p x'

ldc2Insn :: MonadConstantPool m =>
            (a -> m Word16) ->
            a ->
            CodeT s m i j (Label (CodeT s m) i)
ldc2Insn k x = CodeT $ do
  x' <- lift $ k x
  unCodeT $ insn' 2 0 3 $ do
    putWord8 Opcode.ldc2_w
    putWord16be x'
