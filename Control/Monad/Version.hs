module Control.Monad.Version
       ( module Control.Monad.Version.Class
       , Version
       , runVersion
       , VersionT
       , runVersionT
       ) where

type Version a = VersionT Identity a

newtype VersionT m a = VersionT
                       { unVersionT :: ReaderT (Word16, Word16) m a
                       } deriving ( Functor
                                  , Applicative
                                  , Alternative
                                  , Monad
                                  , MonadFix
                                  , MonadPlus
                                  , MonadTrans
                                  , MonadConstantPool
                                  )