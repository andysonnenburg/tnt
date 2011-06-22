{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main) where

import Control.Monad.Code
import Control.Monad.ConstantPool
import Control.Monad.Indexed hiding (return)
import qualified Control.Monad.Indexed as M
import Control.Monad.Version

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.ClassFile
import Data.ClassFile.Access
import Data.Monoid

import System.IO

import Prelude hiding (Monad (..))
import qualified Prelude

main :: IO ()
main =
  hSetBinaryMode stdout True Prelude.>>
  (BL.putStr .
   runPut .
   putClassFile .
   runVersion 0 49 .
   evalConstantPoolT $ hello)
  where
    hello =
      classM (mconcat [public, final, super]) "Main" (Just "java/lang/Object")
      []
      []
      [ execCode
        (mconcat [public, static, final]) "main" (A$L"java/lang/String")V $ do
          getstatic "java/lang/System" "out" $ L"java/io/PrintStream"
          ldc "hello, world"
          invokevirtual "java/io/PrintStream" "println" (L"java/lang/String")V
          return
          M.return ()
      ]
      []