{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Parameterized (Monad (..)) where

import Prelude hiding (Monad (..))

infixl 1 >>, >>=

class Monad m where
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b
  (>>) :: m i j a -> m j k b -> m i k b
  return :: a -> m i i a
  fail :: String -> m i j a
  
  {-# INLINE (>>) #-}
  m >> k = m >>= \_ -> k
  fail = error