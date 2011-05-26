{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Control.Monad.ConstantPool
       ( module Control.Monad.ConstantPool.Class
       , ConstantPool
       , runConstantPool
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ConstantPool.Class
import Control.Monad.State

import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word

import Prelude hiding (lookup)

data CpInfo = Class Word16
            | Fieldref Word16 Word16
            | Methodref Word16 Word16
            | InterfaceMethodref Word16 Word16
            | String Word16
            | Integer Int32
            | Float Float
            | Long Int64
            | Double Double
            | NameAndType Word16 Word16
            | Utf8 String deriving (Show, Eq, Ord)

data S = S { constantPoolCount :: Word16
           , constantPool :: [CpInfo]
           , constantPoolMap :: Map CpInfo Word16
           } 

newtype ConstantPool a = ConstantPool { unConstantPool :: State S a }
                       deriving (Functor, Applicative, Monad, MonadFix)

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

lookupNameAndType :: String -> String -> ConstantPool Word16
lookupNameAndType name dsc = do
  i <- lookupUtf8 name
  j <- lookupUtf8 dsc
  lookup $ NameAndType i j

lookupUtf8 :: String -> ConstantPool Word16
lookupUtf8 = lookup . Utf8

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