{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}
module Control.Monad.Version
       ( module Control.Monad.Version.Class
       , Version
       , runVersion
       , VersionT
       , runVersionT
       ) where

import Control.Applicative
import Control.Monad.ConstantPool.Class
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Version.Class

import Data.Word

type Version = VersionT Identity

runVersion :: Version a -> Word16 -> Word16 -> a
runVersion m minorVersion majorVersion =
  runIdentity $ runVersionT m minorVersion majorVersion

newtype VersionT m a = VersionT
                       { unVersionT :: ReaderT R m a
                       } deriving ( Functor
                                  , Applicative
                                  , Alternative
                                  , Monad
                                  , MonadFix
                                  , MonadPlus
                                  , MonadTrans
                                  )
                                  
data R = R { minorVersion :: {-# UNPACK #-} !Word16
           , majorVersion :: {-# UNPACK #-} !Word16
           }

runVersionT :: VersionT m a -> Word16 -> Word16 -> m a
runVersionT m minorVersion majorVersion =
  runReaderT (unVersionT m) R { minorVersion,  majorVersion }

instance Monad m => MonadVersion (VersionT m) where
  getMinorVersion = VersionT $ liftM minorVersion ask
  getMajorVersion = VersionT $ liftM majorVersion ask