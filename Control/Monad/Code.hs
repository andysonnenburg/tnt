{-# LANGUAGE Rank2Types #-}
module Control.Monad.Code
       ( module Control.Monad.Code.Typed
       , Code
       , execCode
       ) where

import Control.Monad.Code.Typed
import Control.Monad.ConstantPool
import Control.Monad.Version

import Data.ClassFile.Access
import Data.ClassFile.MethodInfo

type Code s = CodeT s (ConstantPoolT Version)

execCode :: ( ParameterDesc parameters
            , ReturnDesc return
            ) =>
            MethodAccess ->
            String ->
            parameters ->
            return ->
            (forall s. Code s () q a) ->
            ConstantPoolT Version MethodInfo
execCode = undefined