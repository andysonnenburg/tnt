{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main) where

import Control.Applicative
import qualified Control.Monad as Monad
import Control.Monad.Code
import Control.Monad.ConstantPool
import Control.Monad.Parameterized hiding (return)

import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.ClassFile
import Data.ClassFile.Access

import Language.Brainfuck.Emitter
import Language.Brainfuck.Parser

import System.IO

import Prelude hiding (Monad (..))

main :: IO ()
main = BL.getContents Monad.>>=
       either (hPutStrLn stderr) (putBinary .
                                  runPut .
                                  putClassFile .
                                  f .
                                  execCode (public .|. final) "run" ()V .
                                  emit) . parse
  where
    putBinary s = hSetBinaryMode stdout True *>
                  BL.putStr s <*
                  hSetBinaryMode stdout False
                  
    initMethod = execCode
      public "<init>" ()V $ do
        aload 0
        invokespecial "java/lang/Object" "<init>" ()V
        return
    
    mainMethod = execCode
      (public .|. static .|. final) "main" (A(L"java/lang/String"))V $ do
        new "Main"
        dup
        invokespecial "Main" "<init>" ()V
        invokevirtual (L"Main") "run" ()V
        return
        
    f runMethod = ClassFile { minorVersion = 0
                            , majorVersion = 50
                            , constantPoolLength
                            , constantPool
                            , accessFlags = public .|. final
                            , thisClass
                            , superClass
                            , interfaces = []
                            , fields = []
                            , methods
                            , attributes = []
                            }
      where
        ((thisClass, superClass, methods), constantPoolLength, constantPool) =
          runConstantPool ((,,)
                           <$> lookupClass "Main"
                           <*> lookupClass "java/lang/Object"
                           <*> sequence [runMethod, initMethod, mainMethod])