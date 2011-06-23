{-# LANGUAGE RankNTypes, RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.Brainfuck.Compiler (compile) where

import Control.Monad hiding (Monad (..))
import Control.Monad.Code
import Control.Monad.ConstantPool
import Control.Monad.Indexed
import Control.Monad.Indexed.Syntax hiding (return)
import Control.Monad.Version

import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import Data.ClassFile
import Data.ClassFile.Access
import Data.Monoid

import Language.Brainfuck.Command
import Language.Brainfuck.Emitter
import Language.Brainfuck.Optimizer
import Language.Brainfuck.Parser

import Prelude hiding (Monad (..))

compile :: String -> ByteString -> Either String ByteString
compile className = liftM f . parse
  where
    f x =
      let x' = (emit' . optimize) x >> returnM ()
          x'' = toClassFile className x'
      in runPut . putClassFile $ x''

toClassFile :: String -> (forall s. Code s () i ()) -> ClassFile
toClassFile className x = runVersion 0 49 $ evalConstantPoolT $
  classM (mconcat [ public
                  , final
                  , super
                  ]) className (Just "java/lang/Object")
    ["java/lang/Runnable"]
    []
    [ execCode
      public "<init>" ()V $ do
        aload 0
        invokespecial "java/lang/Object" "<init>" ()V
        return
        returnM ()
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
        returnM ()
    , execCode (mconcat [public, final]) "run" ()V x
    ]
    []

emit' :: [Command] -> Code s () () (Label (Code s) ())
emit' = emit