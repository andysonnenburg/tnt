{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Parameterized (Monad (..), WrappedMonad (..)) where

import qualified Control.Monad as Monad
import Control.Monad.Fix

import Prelude hiding (Monad (..))

infixl 1 >>, >>=

class Monad m where
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b
  (>>) :: m i j a -> m j k b -> m i k b
  return :: a -> m i i a
  fail :: String -> m i j a
  
  {-# INLINE (>>) #-}
  m >> k = m >>= \_ -> k
  fail s = error s

newtype WrappedMonad m i j a = WrapMonad { unwrapMonad :: m a }

instance Monad.Monad m => Monad.Monad (WrappedMonad m i i) where
  (>>=) = (>>=)
  (>>) = (>>)
  return = return
  fail = fail

instance Monad.Monad m => Monad (WrappedMonad m) where
  WrapMonad m >>= k = WrapMonad (m Monad.>>= (unwrapMonad . k))
  WrapMonad m >> WrapMonad n = WrapMonad (m Monad.>> n)
  return = WrapMonad . Monad.return
  fail = WrapMonad . Monad.fail

instance MonadFix m => MonadFix (WrappedMonad m i i) where
  mfix k = WrapMonad . mfix $ (unwrapMonad . k)