{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.ConstantPool
       ( module Control.Monad.ConstantPool.Class
       , ConstantPool
       , runConstantPool
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ConstantPool.Class
import Control.Monad.State

import Data.ClassFile.CpInfo
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word

import Prelude hiding (lookup)

deriving instance Eq CpInfo
deriving instance Ord CpInfo

data S = S { constantPoolCount :: Word16
           , constantPool :: [CpInfo]
           , constantPoolMap :: Map CpInfo Word16
           } 

newtype ConstantPool a = ConstantPool
                         { unConstantPool :: State S a
                         } deriving (Functor, Applicative, Monad, MonadFix)

runConstantPool :: ConstantPool a -> (a, Word16, [CpInfo])
runConstantPool m = case runState (unConstantPool m) initState of
  (a, S {..}) -> (a, constantPoolCount, reverse constantPool)

initState :: S
initState = S 1 [] Map.empty

instance MonadConstantPool ConstantPool where
  lookupClass = lookupUtf8 >=> lookup . Class
  
  lookupField typ name dsc = do
    i <- lookupClass typ
    j <- lookupNameAndType name dsc
    lookup (Fieldref i j)
  
  lookupMethod typ name dsc = do
    i <- lookupClass typ
    j <- lookupNameAndType name dsc
    lookup (Methodref i j)
    
  lookupInterfaceMethod typ name dsc = do
    i <- lookupClass typ
    j <- lookupNameAndType name dsc
    lookup (InterfaceMethodref i j)
  
  lookupString = lookupUtf8 >=> lookup . String
  
  lookupInteger = lookup . Integer
  
  lookupFloat = lookup . Float
  
  lookupLong = lookup . Long
  
  lookupDouble = lookup . Double
  
  lookupUtf8 = lookup . Utf8

lookupNameAndType :: String -> String -> ConstantPool Word16
lookupNameAndType name dsc = do
  i <- lookupUtf8 name
  j <- lookupUtf8 dsc
  lookup $ NameAndType i j

lookup :: CpInfo -> ConstantPool Word16
lookup x = ConstantPool $ do
  s@(S n xs m) <- get
  case Map.lookup x m of
    Nothing -> do
      put $ S (n + n') (x:xs) (Map.insert x n m)
      return n
    Just i -> do
      put s
      return i
  where
    n' = case x of
      Long _ -> 2
      Double _ -> 2
      _ -> 1