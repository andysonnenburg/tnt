module Control.Monad.Indexed.Class (Monad (..)) where

import Prelude hiding (Monad (..))

infixl 1 `thenM_`, `thenM`

class Monad m where
  thenM :: m i j a -> (a -> m j k b) -> m i k b
  thenM_ :: m i j a -> m j k b -> m i k b
  returnM :: a -> m i i a
  failM :: String -> m i j a
  
  m `thenM_` k = m `thenM` \_ -> k
  {-# INLINE thenM_ #-}
  
  failM = error