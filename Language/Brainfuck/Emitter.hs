{-# LANGUAGE DoRec, NoMonomorphismRestriction, RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
module Language.Brainfuck.Emitter (emit) where

import Control.Monad (forM_)
import Control.Monad.Fix
import Control.Monad.Code.Class
import Control.Monad.Indexed hiding (return)
import qualified Control.Monad.Indexed as M

import Language.Brainfuck.Command

import Prelude hiding (Monad (..))

emit :: ( MonadCode m
        , MonadFix (m () ())
        ) => [Command] -> m () () (Label m ())
emit xs = do
  label <- emitHeader
  forM_ xs emitCommand
  emitFooter
  M.return label

emitHeader :: MonadCode m => m i i (Label m i)
emitHeader = do
  label <- ldc 30000
  newarray T_BYTE
  astore 1
  ldc 0
  istore 2
  M.return label

emitCommand :: (MonadCode m, MonadFix (m i i)) => Command -> m i i (Label m i)
emitCommand (IncrementPointer x) = iinc 2 x
emitCommand (IncrementByte x) = do
  label <- aload 1
  iload 2
  dup2
  baload
  ldc x
  iadd
  i2b
  bastore
  M.return label
emitCommand OutputByte = do
  label <- getstatic "java/lang/System" "out" (L"java/io/PrintStream")
  aload 1
  iload 2
  baload
  invokevirtual "java/io/PrintStream" "write" (I)V
  M.return label
emitCommand InputByte = do
  label <- aload 1
  iload 2
  getstatic "java/lang/System" "in" (L"java/io/InputStream")
  invokevirtual "java/io/InputStream" "read" ()I
  i2b
  bastore
  M.return label
emitCommand (WhileNonzero xs) = do
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
{- emitCommand (WhileNonzero xs) =
  do
    rec
      start <- aload 1
      iload 2
      baload
      ifeq end
      forM_ xs emitCommand
      goto start
      end <- nop
    M.return start -}

emitFooter :: MonadCode m => m i i (Label m i)
emitFooter = do
  getstatic "java/lang/System" "out" (L"java/io/PrintStream")
  invokevirtual "java/io/PrintStream" "flush" ()V
  return