{-# LANGUAGE DoRec #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Code
       ( module Control.Monad.Code.Class
       , Control.Monad.Code.Class.Int (..)
       , CodeT
       , runCode
       , execCode
       ) where

import Control.Applicative hiding (Const)
import Control.Monad
import qualified Control.Monad as Monad
import Control.Monad.Code.Class hiding (Int (..), return)
import qualified Control.Monad.Code.Class
import qualified Control.Monad.Code.Opcode as Opcode
import Control.Monad.ConstantPool.Class
import Control.Monad.Fix
import qualified Control.Monad.Parameterized as Parameterized
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Array.Unboxed
import Data.Binary.Builder
import Data.ClassFile.Attribute
import Data.Int
import Data.List
import Data.Ord
import Data.Word

import Prelude hiding (Double, Float, Int, Integer)

data Expr = Expr Int32 ([Word16] -> [Word16]) ([Word16] -> [Word16])

instance Show Expr where
  showsPrec p (Expr a b c) = showParen (p > 10) $ showString "Expr " .
                             shows a . showChar ' ' .
                             shows (b []) . showChar ' ' .
                             shows (c [])

instance Eq Expr where
  (Expr a b c) == (Expr x y z) = a == x && b [] == y [] && c [] == z []

instance Num Expr where
  (Expr a b c) + (Expr x y z) = Expr (a + x) (b . y) (c . z)
  (*) = undefined
  (Expr a b c) - (Expr x y z) = Expr (a - x) (b . z) (c . y)
  fromInteger x = Expr (fromIntegral x) id id
  abs = undefined
  signum = undefined

eval (Expr a b c) = do
  env <- ask
  return $ a + sum (map (env !) (b [])) - sum (map (env !) (c []))

type Eval = WriterT Builder (ReaderT (UArray Word16 Int32) [])

data S = S { stack :: Word16
           , maxStack :: Word16
           , maxLocals :: Word16
           , varCount :: Word16
           , varDomain :: [[Int32]]
           , codeLength :: Expr
           , code :: Eval ()
           }

newtype CodeT m i j a = CodeT { unCodeT :: StateT S m a }

deriving instance Functor m => Functor (CodeT m i i)
deriving instance (Functor m, Monad m) => Applicative (CodeT m i i)
deriving instance Monad m => Monad (CodeT m i i)
deriving instance MonadFix m => MonadFix (CodeT m i i)

runCode :: Monad m => CodeT m () i a -> m (a, Attribute)
runCode (CodeT m) = do
  (a, S {..}) <- runStateT m initState
  let x = snd . head . sortBy (comparing fst) $ do
          let varDomain' = reverse varDomain
              code' = code >> eval codeLength
          env <- listArray (0, varCount - 1) <$> f varDomain'
          (runReaderT . runWriterT) code' env
  Monad.return (a, Attribute "CodeAttribute" (toLazyByteString x))
  where
    f [] = [[]]
    f (x:xs) = [y:ys | y <- x, ys <- f xs]

execCode :: Monad m => CodeT m () i a -> m Attribute
execCode = liftM snd . runCode

initState :: S
initState = S 0 0 0 0 [] 0 (return ())

newVar :: MonadState S m => [Int32] -> m Expr
newVar xs = do
  s@S {..} <- get
  put $ s { varCount = varCount + 1
          , varDomain = xs:varDomain
          }
  return $ Expr 0 (varCount:) id

instance Monad m => Parameterized.Monad (CodeT m) where
  return = CodeT . Monad.return

  m >>= k = CodeT (unCodeT m >>= unCodeT . k)

instance MonadConstantPool m => MonadCode (CodeT m) where
  
  newtype Label (CodeT m) xs = Label Expr

  aaload = simpleInsn (-2) Opcode.aaload
  aastore = simpleInsn (-3) Opcode.aastore
  aconst_null = simpleInsn 1 Opcode.aconst_null

  aload j = case j of
    0 -> insn 1 1 1 (tell $ singleton Opcode.aload_0)
    1 -> insn 1 2 1 (tell $ singleton Opcode.aload_1)
    2 -> insn 1 3 1 (tell $ singleton Opcode.aload_2)
    3 -> insn 1 4 1 (tell $ singleton Opcode.aload_3)
    _ -> varInsn 1 j Opcode.aload

  astore j = case j of
    0 -> insn (-1) 1 1 (tell $ singleton Opcode.astore_0)
    1 -> insn (-1) 2 1 (tell $ singleton Opcode.astore_1)
    2 -> insn (-1) 3 1 (tell $ singleton Opcode.astore_2)
    3 -> insn (-1) 4 1 (tell $ singleton Opcode.astore_3)
    _ -> varInsn (-1) j Opcode.astore

  baload = simpleInsn (-2) Opcode.baload

  bastore = simpleInsn (-3) Opcode.bastore

  dup2 = simpleInsn 2 Opcode.dup2

  getstatic typ name fieldType = CodeT $ do
    x <- lift (lookupField typ name (desc fieldType))
    unCodeT (insn (stackSize fieldType) 0 3 $ do
      tell $ singleton Opcode.getstatic
      tell $ putWord16be x)

  goto (Label target) = CodeT $ do
    size <- newVar [3, 5]
    s@S {..} <- get
    let m = do
          size' <- eval size
          codeLength' <- eval codeLength
          target' <- eval target
          let offset = target' - codeLength'
              narrow = offset >= fromIntegral (minBound :: Int16) &&
                       offset <= fromIntegral (maxBound :: Int16)
          guard (size' == 3 && narrow ||
                 size' == 5 && not narrow)
          if narrow
            then do tell $ singleton Opcode.goto
                    tell $ putWord16be . fromIntegral $ offset
            else do tell $ singleton Opcode.goto_w
                    tell $ putWord32be . fromIntegral $ offset
    put $ s { codeLength = codeLength + size
            , code = code >> m
            }
    return . Label $ codeLength

  i2b = simpleInsn 0 Opcode.i2b
  
  iadd = simpleInsn (-1) Opcode.iadd

  ifeq (Label target) = CodeT $ do
    size <- newVar [3, 8]
    s@S {..} <- get
    let m = do
          size' <- eval size
          codeLength' <- eval codeLength
          target' <- eval target
          let offset = target' - codeLength'
              narrow = offset >= fromIntegral (minBound :: Int16) &&
                       offset <= fromIntegral (maxBound :: Int16)
          guard (size' == 3 && narrow ||
                 size' == 8 && not narrow)
          if narrow
            then do tell $ singleton Opcode.ifne
                    tell $ putWord16be . fromIntegral $ offset
            else do tell $ singleton Opcode.ifeq
                    tell $ putWord16be 8
                    tell $ singleton Opcode.goto_w
                    tell $ putWord32be . fromIntegral $ offset
    put $ s { stack = stack - 1
            , codeLength = codeLength + size
            , code = code >> m
            }
    return . Label $ codeLength
  
  iinc j cnst =
    if j <= fromIntegral (maxBound :: Word8) &&
       cnst >= fromIntegral (minBound :: Int8) &&
       cnst <= fromIntegral (maxBound :: Int8) then
      insn 0 j 3 $ do
        tell $ singleton Opcode.iinc
        tell $ singleton . fromIntegral $ j
        tell $ singleton . fromIntegral $ cnst
    else
      insn 0 j 6 $ do
        tell $ singleton Opcode.wide
        tell $ singleton Opcode.iinc
        tell $ putWord16be j
        tell $ putWord16be . fromIntegral $ cnst

  iload j = case j of
    0 -> insn 1 1 1 (tell $ singleton Opcode.iload_0)
    1 -> insn 1 2 1 (tell $ singleton Opcode.iload_1)
    2 -> insn 1 3 1 (tell $ singleton Opcode.iload_2)
    3 -> insn 1 4 1 (tell $ singleton Opcode.iload_3)
    _ -> varInsn 1 j Opcode.iload

  istore j = case j of
    0 -> insn (-1) 1 1 (tell $ singleton Opcode.istore_0)
    1 -> insn (-1) 2 1 (tell $ singleton Opcode.istore_1)
    2 -> insn (-1) 3 1 (tell $ singleton Opcode.istore_2)
    3 -> insn (-1) 4 1 (tell $ singleton Opcode.istore_3)
    _ -> varInsn (-1) j Opcode.istore

  invokevirtual typ method parameters result = CodeT $ do
    x <- lift (lookupField (internalName typ) method dsc)
    unCodeT (insn i 0 3 $ do
      tell $ singleton Opcode.invokevirtual
      tell $ putWord16be x)
    where
      dsc = methodDesc parameters result
      i = stackSize result - stackSize parameters

  isub = simpleInsn (-1) Opcode.isub

  ldcInt x = case x of
    (-1) -> simpleInsn 1 Opcode.iconst_m1
    0 -> simpleInsn 1 Opcode.iconst_0
    1 -> simpleInsn 1 Opcode.iconst_1
    2 -> simpleInsn 1 Opcode.iconst_2
    3 -> simpleInsn 1 Opcode.iconst_3
    4 -> simpleInsn 1 Opcode.iconst_4
    5 -> simpleInsn 1 Opcode.iconst_5
    _
      | x >= fromIntegral (minBound :: Int8) &&
        x <= fromIntegral (maxBound :: Int8) -> insn 1 0 2 $ do
          tell $ singleton Opcode.bipush
          tell $ singleton . fromIntegral $ x
      | x >= fromIntegral (minBound :: Int16) &&
        x <= fromIntegral (maxBound :: Int16) -> insn 1 0 3 $ do
          tell $ singleton Opcode.sipush
          tell $ putWord16be . fromIntegral $ x
      | otherwise -> ldcInsn lookupInteger x

  ldcFloat = ldcInsn lookupFloat
  ldcString = ldcInsn lookupString
  ldcClass = ldcInsn lookupClass

  ldcLong = ldc2Insn lookupLong
  ldcDouble = ldc2Insn lookupDouble

  newarray typ = insn 0 0 2 $ do
    tell $ singleton Opcode.newarray
    tell $ singleton $ case typ of
      T_BOOLEAN -> 4
      T_CHAR -> 5
      T_FLOAT -> 6
      T_DOUBLE -> 7
      T_BYTE -> 8
      T_SHORT -> 9
      T_INT -> 10
      T_LONG -> 11

  nop = CodeT $ liftM (Label . codeLength) get

  return = CodeT $ do
    s@S {..} <- get
    put $ s { codeLength = codeLength + 1
            , code = do code
                        tell $ singleton Opcode.return
            }
    return . Label $ codeLength

insn :: Monad m =>
        Word16 ->
        Word16 ->
        Expr ->
        Eval () ->
        CodeT m i j (Label (CodeT m) i)
insn i j n m = CodeT $ do
  s@S {..} <- get
  let stack' = stack + i
  put $ s { stack = stack'
          , maxStack = max maxStack stack'
          , maxLocals = max maxLocals j
          , codeLength = codeLength + n
          , code = code >> m
          }
  return . Label $ codeLength

simpleInsn :: Monad m => Word16 -> Word8 -> CodeT m i j (Label (CodeT m) i)
simpleInsn i o = insn i 0 1 (tell $ singleton o)

varInsn :: Monad m =>
           Word16 ->
           Word16 ->
           Word8 ->
           CodeT m i j (Label (CodeT m) i)
varInsn i j o
  | j < fromIntegral (maxBound :: Word8) = insn i (j + 1) 2 $ do
    tell $ singleton o
    tell $ singleton . fromIntegral $ j
  | otherwise = insn i (j + 1) 3 $ do
    tell $ singleton Opcode.wide
    tell $ singleton o
    tell $ putWord16be j

ldcInsn :: MonadConstantPool m =>
           (a -> m Word16) ->
           a ->
           CodeT m i j (Label (CodeT m) i)
ldcInsn k x = CodeT $ do
  x' <- lift (k x)
  let (n, p, o) = if x' < fromIntegral (maxBound :: Word8)
                  then (2, singleton . fromIntegral, Opcode.ldc)
                  else (3, putWord16be, Opcode.ldc_w)
  unCodeT (insn 1 0 n $ do
    tell $ singleton o
    tell $ p x')

ldc2Insn :: MonadConstantPool m =>
            (a -> m Word16) ->
            a ->
            CodeT m i j (Label (CodeT m) i)
ldc2Insn k x = CodeT $ do
  x' <- lift (k x)
  unCodeT (insn 2 0 3 $ do
    tell $ singleton Opcode.ldc2_w
    tell $ putWord16be x')
