{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main) where

import Control.Applicative
import qualified Control.Monad as Monad
import Control.Monad.Code
import Control.Monad.Indexed hiding (return)

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.ClassFile
import Data.ClassFile.Access

import Language.Brainfuck.Emitter
import Language.Brainfuck.Optimizer
import Language.Brainfuck.Parser

import System.IO

import Prelude hiding (Monad (..))

main :: IO ()
main = BL.getContents Monad.>>=
       either
       (hPutStrLn stderr)
       (putBinary .
        runPut .
        putClassFile .
        toClassFile .
        emit .
        optimize) .
       parse
  where
    putBinary s = hSetBinaryMode stdout True *>
                  BL.putStr s <*
                  hSetBinaryMode stdout False
    
    toClassFile x =
      classM (fromList [public, final]) "Main" (Just "java/lang/Object")
      []
      []
      [ execCode
        (fromList [public]) "<init>" ()V $ do
          aload 0
          invokespecial "java/lang/Object" "<init>" ()V
          return
      
      , execCode
        (fromList [ public
                  , static
                  , final
                  ]) "main" (A$L"java/lang/String")V $ do
          new "Main"
          dup
          invokespecial "Main" "<init>" ()V
          invokevirtual (L"Main") "run" ()V
          return
      
      , execCode (fromList [public, final]) "run" ()V x
      
      ]
      []