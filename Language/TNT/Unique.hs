{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , RankNTypes
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.TNT.Unique
       ( module Language.TNT.Unique.Class
       , Unique
       , uniqueToWord
       , UniqueT
       , runUniqueT
       ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Word

import Language.TNT.Unique.Class

newtype Unique = Unique Word deriving Show

uniqueToWord :: Unique -> Word
uniqueToWord (Unique x) = x

newtype UniqueT m a = UniqueT
                      { unUniqueT :: StateT Word m a
                      } deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , MonadTrans
                                 )

deriving instance MonadError e m => MonadError e (UniqueT m)
deriving instance MonadReader r m => MonadReader r (UniqueT m)

instance MonadState s m => MonadState s (UniqueT m) where
  get = UniqueT . lift $ get
  put = UniqueT . lift . put

runUniqueT :: Monad m => UniqueT m a -> m a
runUniqueT = flip evalStateT 0 . unUniqueT

instance Monad m => MonadUnique Unique (UniqueT m) where
  newUnique = UniqueT $ do
    i <- get
    put $ i + 1
    return . Unique $ i