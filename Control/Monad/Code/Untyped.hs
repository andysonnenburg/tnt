module Control.Monad.Code.Untyped
       ( Code
       , runCode
       , CodeT
       , runCodeT
       ) where

import qualified Control.Monad.Code.Internal as Internal
import Control.Monad.Code.Class.Untyped

import Data.ClassFile.Desc.Untyped

newtype CodeT s a = CodeT (Internal.CodeT s a)

instance Monad m => MonadClass (CodeT m) where
  type ArrayType m = Internal.ArrayType
  type Label m = Internal.Label

