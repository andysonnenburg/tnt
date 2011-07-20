{-# LANGUAGE RankNTypes, RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.TNT.Compiler (compile) where

import Control.Applicative
-- import Control.Monad.Code
import Control.Monad.ConstantPool
import Control.Monad.Identity
import Control.Monad.State hiding (Monad (..))
import Control.Monad.Version

import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ClassFile
import Data.ClassFile.Access

import Language.TNT.ClosureLifter
-- import Language.TNT.Emitter
import Language.TNT.Error
-- import Language.TNT.LambdaLifter
import Language.TNT.Lexer
import Language.TNT.Location
import Language.TNT.Name
import Language.TNT.Namer
import Language.TNT.Parser
import Language.TNT.Scope
import Language.TNT.Stmt

import Prelude

compile :: String ->
           String ->
           Either (Located String) (Dec Located Name)
compile cn = runIdentity . runErrorT . f
  where
    f s = do
      a <- parse s
      let x = FunD cn [] a
      y <- runScopeT . name $ x
      return . closureLift $ y
  -- where
  --   f = runPut .
  --       putClassFile .
  --       toClassFile className .
  --       emit

-- toClassFile className x = runVersion 0 49 . evalStateT . evalConstantPoolT $
--   classM (mconcat [ public
--                   , final
--                   , super]) className (Just "java/lang/Object")
--     ["java/lang/Runnable"]
--     []
--     [ execCode
--       public "<init>" ()V $ do
--         aload 0
--         invokespecial "java/lang/Object" "<init>" ()V
--         return
--     , execCode
--       (mconcat [ public
--                , static
--                , final
--                ]) "main" (A$L"java/lang/String")V $ do
--         new className
--         dup
--         invokespecial className "<init>" ()V
--         invokevirtual className "run" ()V
--         return
--     , execCode (mconcat [public, final]) "run" ()V x
--     ]
--     []