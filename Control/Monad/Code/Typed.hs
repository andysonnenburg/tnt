{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Control.Monad.Code.Typed
       ( module Control.Monad.Code.Class.Typed
       , Typed.Label
       , Typed.return
       , CodeT
       , runCodeT
       ) where

import Control.Applicative
import Control.Monad.Code.Class.Typed hiding (Label, return)
import qualified Control.Monad.Code.Class.Typed as Typed
import qualified Control.Monad.Code.Internal as Internal
import Control.Monad.Fix
import Control.Monad.Indexed hiding (Monad)
import qualified Control.Monad.Indexed as Indexed

newtype CodeT s m p q a = CodeT
                          { unCodeT :: Internal.CodeT s m a
                          } deriving ( Functor
                                     , Applicative
                                     , Monad
                                     , MonadFix
                                     )

runCodeT = undefined

instance Monad m => Indexed.Monad (CodeT s m) where
  returnM = CodeT . return
  {-# INLINE returnM #-}
  
  m `thenM` k = CodeT $ unCodeT m >>= unCodeT . k
  {-# INLINE thenM #-}
  
  failM = CodeT . fail

newtype Label q = Label Internal.Label

instance Monad m => MonadCode (CodeT s m) where
  newtype ArrayType (CodeT s m) = ArrayType Internal.ArrayType
  type Typed.Label (CodeT s m) = Label