{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
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

runVersion :: Word16 -> Word16 -> Version a -> a
runVersion minorVersion majorVersion m =
  runIdentity $ runVersionT minorVersion majorVersion m

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

runVersionT :: Word16 -> Word16 -> VersionT m a -> m a
runVersionT minorVersion majorVersion m =
  runReaderT (unVersionT m) R {..}

instance Monad m => MonadVersion (VersionT m) where
  getMinorVersion = VersionT $ liftM minorVersion ask
  getMajorVersion = VersionT $ liftM majorVersion ask