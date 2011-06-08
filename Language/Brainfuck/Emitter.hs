{-# LANGUAGE DoRec, NoMonomorphismRestriction, RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
module Language.Brainfuck.Emitter (emit) where

import Control.Monad (forM_)
import Control.Monad.Fix
import Control.Monad.Code
import Control.Monad.Indexed hiding (return)
import qualified Control.Monad.Indexed as M

import Data.Int

import Language.Brainfuck.Command

import Prelude hiding (Monad (..))

type Emit i = forall m. (MonadFix (m i i), MonadCode m) => m i i (Label m i)
-- type Emit i = forall s. Code s i i (Label (Code s) i)

emit :: [Command] -> Emit ()
emit xs = do
  label <- emitHeader
  forM_ xs emitCommand
  emitFooter
  M.return label

emitHeader :: Emit i
emitHeader = do
  label <- ldc 30000
  newarray T_BYTE
  astore 1
  ldc 0
  istore 2
  M.return label

emitCommand :: Command -> Emit i
emitCommand (IncrementPointer x) = emitIncrementPointer x
emitCommand (IncrementByte x) = emitIncrementByte x
emitCommand OutputByte = emitOutputByte
emitCommand InputByte = emitInputByte
emitCommand (WhileNonzero xs) = emitWhileNonzero xs

emitIncrementPointer :: Int32 -> Emit i
emitIncrementPointer = iinc 2

emitIncrementByte :: Int32 -> Emit i
emitIncrementByte x = do
  label <- aload 1
  iload 2
  dup2
  baload
  ldc x
  iadd
  i2b
  bastore
  M.return label

emitOutputByte :: Emit i
emitOutputByte = do
  label <- getstatic "java/lang/System" "out" (L"java/io/PrintStream")
  aload 1
  iload 2
  baload
  invokevirtual "java/io/PrintStream" "write" (I)V
  M.return label

emitInputByte :: Emit i
emitInputByte = do
  label <- aload 1
  iload 2
  getstatic "java/lang/System" "in" (L"java/io/InputStream")
  invokevirtual "java/io/InputStream" "read" ()I
  i2b
  bastore
  M.return label

emitWhileNonzero :: [Command] -> Emit i
emitWhileNonzero xs = do
  (start, _) <- mfix (\ ~(_, end) -> do
    start <- aload 1
    iload 2
    baload
    ifeq end
    forM_ xs emitCommand
    goto start
    end <- nop
    M.return (start, end))
  M.return start

emitFooter :: Emit i
emitFooter = do
  getstatic "java/lang/System" "out" (L"java/io/PrintStream")
  invokevirtual "java/io/PrintStream" "flush" ()V
  return