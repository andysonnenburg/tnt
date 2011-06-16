{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.ConstantPool
       ( module Control.Monad.ConstantPool.Class
       , ConstantPool
       , runConstantPool
       , evalConstantPool
       , ConstantPoolT
       , runConstantPoolT
       , evalConstantPoolT
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ConstantPool.Class
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.Version.Class
import Control.Monad.Writer.Class

import Data.ClassFile.CpInfo
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word

import Prelude hiding (lookup)

type ConstantPool = ConstantPoolT Identity

runConstantPool :: ConstantPool a -> (a, Word16, [CpInfo])
runConstantPool = runIdentity . runConstantPoolT

evalConstantPool :: ConstantPool a -> a
evalConstantPool = runIdentity . evalConstantPoolT

data S = S {-# UNPACK #-} !Word16 ![CpInfo] !(Map CpInfo Word16)

newtype ConstantPoolT m a = ConstantPoolT
                            { unConstantPoolT :: StateT S m a
                            } deriving ( Functor
                                       , Applicative
                                       , Alternative
                                       , Monad
                                       , MonadFix
                                       , MonadPlus
                                       , MonadTrans
                                       , MonadIO
                                       , MonadCont
                                       )

deriving instance MonadError e m => MonadError e (ConstantPoolT m)
deriving instance MonadReader r m => MonadReader r (ConstantPoolT m)
deriving instance MonadWriter w m => MonadWriter w (ConstantPoolT m)

instance MonadVersion m => MonadVersion (ConstantPoolT m) where
  getMinorVersion = ConstantPoolT . lift $ getMinorVersion
  getMajorVersion = ConstantPoolT . lift $ getMajorVersion

runConstantPoolT :: Monad m => ConstantPoolT m a -> m (a, Word16, [CpInfo])
runConstantPoolT m = flip evalStateT initState . unConstantPoolT $
                     liftM3 (,,) m getCount getTable

evalConstantPoolT :: Monad m => ConstantPoolT m a -> m a
evalConstantPoolT m = do
  (a, _, _) <- runConstantPoolT m
  return a

initState :: S
initState = S 1 [] Map.empty

instance Monad m => MonadConstantPool (ConstantPoolT m) where
  
  type Table (ConstantPoolT m) = [CpInfo]
  
  getCount = ConstantPoolT $ do
    S count _ _ <- get
    return count
  
  getTable = ConstantPoolT $ do
    S _ table _ <- get
    return . reverse $ table
  
  lookupClass = lookupUtf8 >=> lookup . Class
  
  lookupField typ name dsc = do
    i <- lookupClass typ
    j <- lookupNameAndType name dsc
    lookup $! Fieldref i j
  
  lookupMethod typ name dsc = do
    i <- lookupClass typ
    j <- lookupNameAndType name dsc
    lookup $! Methodref i j
    
  lookupInterfaceMethod typ name dsc = do
    i <- lookupClass typ
    j <- lookupNameAndType name dsc
    lookup $! InterfaceMethodref i j
  
  lookupString = lookupUtf8 >=> lookup . String
  
  lookupInteger = lookup . Integer
  
  lookupFloat = lookup . Float
  
  lookupLong = lookup . Long
  
  lookupDouble = lookup . Double
  
  lookupUtf8 = lookup . Utf8

lookupNameAndType :: Monad m => String -> String -> ConstantPoolT m Word16
lookupNameAndType name dsc = do
  i <- lookupUtf8 name
  j <- lookupUtf8 dsc
  lookup $! NameAndType i j

lookup :: Monad m => CpInfo -> ConstantPoolT m Word16
lookup x = ConstantPoolT $ do
  S n xs m <- get
  case Map.lookup x m of
    Nothing -> do
      put $! S (n + n') (x:xs) (Map.insert x n m)
      return n
    Just i -> 
      return i
  where
    n' = case x of
      Long _ -> 2
      Double _ -> 2
      _ -> 1