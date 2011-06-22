{-# LANGUAGE RankNTypes, RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.Brainfuck.Compiler (compile) where

import Control.Monad hiding (Monad (..))
import Control.Monad.Code
import Control.Monad.ConstantPool
import Control.Monad.Indexed hiding (return)
import qualified Control.Monad.Indexed as M
import Control.Monad.Version

import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import Data.ClassFile
import Data.ClassFile.Access
import Data.Monoid

import Language.Brainfuck.Emitter
import Language.Brainfuck.Optimizer
import Language.Brainfuck.Parser

import Prelude hiding (Monad (..))

compile :: String -> ByteString -> Either String ByteString
compile className = liftM f . parse
  where
    f = runPut .
        putClassFile .
        toClassFile className .
        emit .
        optimize

toClassFile :: forall i a. String -> (forall s. Code s () i a) -> ClassFile
toClassFile className x = runVersion 0 49 $ evalConstantPoolT $
  classM (mconcat [ public
                  , final
                  , super]) className (Just "java/lang/Object")
    ["java/lang/Runnable"]
    []
    [ execCode
      public "<init>" ()V $ do
        aload 0
        invokespecial "java/lang/Object" "<init>" ()V
        return
        M.return ()
    , execCode
      (mconcat [ public
               , static
               , final
               ]) "main" (A$L"java/lang/String")V $ do
        new className
        dup
        invokespecial className "<init>" ()V
        invokevirtual className "run" ()V
        return
        M.return ()
    , execCode (mconcat [public, final]) "run" ()V x
    ]
    []