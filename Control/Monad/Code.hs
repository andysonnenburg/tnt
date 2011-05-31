{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DoRec #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Code
       ( module Control.Monad.Code.Class
       , Control.Monad.Code.Class.Int (..)
       , Control.Monad.Code.Class.return
       , CodeT
       , runCode
       , execCode
       ) where

import Control.Applicative hiding (Const)
import Control.Monad hiding (return)
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
import Data.ClassFile.CodeAttribute (CodeAttribute (CodeAttribute))
import qualified Data.ClassFile.CodeAttribute
import Data.ClassFile.MethodInfo
import Data.Graph
import Data.Int
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Ord
import Data.Word

import Prelude hiding (Double, Float, Int, Integer, return)

data Expr = Expr Int32 (IntMap Int32) deriving (Show, Eq)

instance Num Expr where
  (Expr a b) + (Expr x y) = Expr (a + x) (IntMap.unionWith (+) b y)
  (*) = undefined
  (Expr a b) - (Expr x y) = Expr (a - x) (IntMap.unionWith (+) b y')
    where y' = IntMap.map negate y
  fromInteger x = Expr (fromIntegral x) IntMap.empty
  abs = undefined
  signum = undefined

eval (Expr x y) = do
  env <- ask
  let f _ 0 b = b
      f k a b = (env ! k) * a + b
  return $ x + IntMap.foldWithKey f 0 y

type Eval = WriterT Builder (ReaderT (UArray Int Int32) [])

data S = S { stack :: Word16
           , maxStack :: Word16
           , maxLocals :: Word16
           , varCount :: Int
           , varDomain :: [[Int32]]
           , codeLength :: Expr
           , code :: Eval ()
           }

newtype CodeT m i j a = CodeT { unCodeT :: StateT S m a }

deriving instance Functor m => Functor (CodeT m i i)
deriving instance (Functor m, Monad m) => Applicative (CodeT m i i)
deriving instance Monad m => Monad (CodeT m i i)
deriving instance MonadFix m => MonadFix (CodeT m i i)

runCode :: ( ParameterDesc parameters
           , ReturnDesc result
           , MonadConstantPool m
           ) =>
           Word16 ->
           String ->
           parameters ->
           result ->
           CodeT m () i a -> m (a, MethodInfo)
runCode accessFlags name args result (CodeT m) = do
  nameIndex <- lookupUtf8 name
  descriptorIndex <- lookupUtf8 (methodDesc args result)
  (a, S {..}) <- runStateT m initState
  let varDomain' = reverse varDomain
      code' = code >> eval codeLength
  let x = snd . head . sortBy (comparing fst) $ do
        env <- listArray (0, varCount - 1) <$> f varDomain'
        (runReaderT . runWriterT) code' env
  attr <- toAttributeInfo CodeAttribute { maxStack
                                        , maxLocals = maxLocals + stackSize args
                                        , code = toLazyByteString x
                                        , exceptionTable = []
                                        , attributes = []
                                        }
  Monad.return (a, MethodInfo { accessFlags
                              , nameIndex
                              , descriptorIndex
                              , attributes = [attr]
                              })
  where
    f [] = [[]]
    f (x:xs) = [y:ys | y <- x, ys <- f xs]

execCode :: ( ParameterDesc parameters
            , ReturnDesc result
            , MonadConstantPool m
            ) =>
            Word16 ->
            String ->
            parameters ->
            result ->
            CodeT m () i a -> m MethodInfo
execCode access name parameters result =
  liftM snd . runCode access name parameters result

initState :: S
initState = S { stack = 0
              , maxStack = 0
              , maxLocals = 0
              , varCount = 0
              , varDomain = []
              , codeLength = 0
              , code = return ()
              }

newVar :: MonadState S m => [Int32] -> m Expr
newVar xs = do
  s@S {..} <- get
  put $ s { varCount = varCount + 1
          , varDomain = xs:varDomain
          }
  return $ Expr 0 (IntMap.singleton varCount 1)

instance Monad m => Parameterized.Monad (CodeT m) where
  return = CodeT . Monad.return

  m >>= k = CodeT (unCodeT m >>= unCodeT . k)

instance MonadConstantPool m => MonadCode (CodeT m) where
  
  newtype Label (CodeT m) xs = Label Expr

  aaload = simpleInsn (-1) Opcode.aaload
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

  baload = simpleInsn (-1) Opcode.baload

  bastore = simpleInsn (-3) Opcode.bastore

  dup = simpleInsn 1 Opcode.dup

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
            then do tell $ singleton Opcode.ifeq
                    tell $ putWord16be . fromIntegral $ offset
            else do tell $ singleton Opcode.ifne
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

  invokeinterface typ method args result = CodeT $ do
    x <- lift $ lookupInterfaceMethod typ method dsc
    unCodeT (insn i 0 5 $ do
                tell $ singleton Opcode.invokeinterface
                tell $ putWord16be x
                tell $ singleton . (+ 1) . fromIntegral . stackSize $ args
                tell $ singleton 0)
    where
      dsc = methodDesc args result
      i = stackSize result - stackSize args

  invokespecial typ method args result = CodeT $ do
    x <- lift $ lookupMethod typ method dsc
    unCodeT (insn i 0 3 $ do
                tell $ singleton Opcode.invokespecial
                tell $ putWord16be x)
    where
      dsc = methodDesc args result
      i = stackSize result - stackSize args

  invokevirtual typ method args result = CodeT $ do
    x <- lift $ lookupMethod (internalName typ) method dsc
    unCodeT (insn i 0 3 $ do
                tell $ singleton Opcode.invokevirtual
                tell $ putWord16be x)
    where
      dsc = methodDesc args result
      i = stackSize result - stackSize args

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

  ldcFloat x = case x of
    0 -> simpleInsn 1 Opcode.fconst_0
    1 -> simpleInsn 1 Opcode.fconst_1
    2 -> simpleInsn 1 Opcode.fconst_2
    _ -> ldcInsn lookupFloat x

  ldcString = ldcInsn lookupString

  ldcClass = ldcInsn lookupClass

  ldcLong x = case x of
    0 -> simpleInsn 2 Opcode.lconst_0
    1 -> simpleInsn 2 Opcode.lconst_1
    _ -> ldc2Insn lookupLong x

  ldcDouble x = case x of
    0 -> simpleInsn 2 Opcode.dconst_0
    1 -> simpleInsn 2 Opcode.dconst_1
    _ -> ldc2Insn lookupDouble x

  new typ = CodeT $ do
    x <- lift $ lookupClass typ
    unCodeT (insn 1 0 3 $ do
                tell $ singleton Opcode.new
                tell $ putWord16be x)

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
