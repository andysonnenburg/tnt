{-# LANGUAGE DoRec, NoMonomorphismRestriction, RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.Brainfuck.Emitter (emit) where

import Control.Applicative
import Control.Monad (forM_)
import Control.Monad.Fix
import Control.Monad.Code.Class
import Control.Monad.Parameterized hiding (return)
import qualified Control.Monad.Parameterized as M

import Language.Brainfuck.Command

import Prelude hiding (Monad (..))

emit :: ( Functor (m () ())
        , MonadCode m
        , MonadFix (m () ())
        ) => [Command] -> m () () (Label m ())
emit xs = do
  label <- emitHeader
  forM_ xs emitCommand
  emitFooter
  M.return label

emitHeader = do
  label <- ldc 30000
  newarray T_BYTE
  astore 1
  ldc 0
  istore 2
  M.return label

emitCommand IncrementPointer = iinc 2 1
emitCommand DecrementPointer = iinc 2 (-1)
emitCommand IncrementByte = do
  label <- aload 1
  iload 2
  dup2
  baload
  ldc 1
  iadd
  i2b
  bastore
  M.return label
emitCommand DecrementByte = do
  label <- aload 1
  iload 2
  dup2
  baload
  ldc 1
  isub
  i2b
  bastore
  M.return label
emitCommand OutputByte = do
  label <- getstatic "java/lang/System" "out" (L"java/io/PrintStream")
  aload 1
  iload 2
  baload
  invokevirtual (L"java/io/PrintStream") "write" (I)V
  M.return label
emitCommand InputByte = do
  label <- aload 1
  iload 2
  getstatic "java/lang/System" "in" (L"java/io/InputStream")
  invokevirtual (L"java/io/InputStream") "read" ()I
  i2b
  bastore
  M.return label
emitCommand (WhileNonzero xs) =
  fst <$> mfix (\ ~(_, end) -> do
    start <- aload 1
    iload 2
    baload
    ifeq end
    forM_ xs emitCommand
    goto start
    end <- nop
    M.return (start, end))
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
  

emitFooter = do
  getstatic "java/lang/System" "out" (L"java/io/PrintStream")
  invokevirtual (L"java/io/PrintStream") "flush" ()V
  return