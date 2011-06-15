{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Indexed.Class (Monad (..)) where

import Prelude hiding (Monad (..))

infixl 1 `ithen`, `ibind`

class Monad m where
  ibind :: m i j a -> (a -> m j k b) -> m i k b
  ithen :: m i j a -> m j k b -> m i k b
  ireturn :: a -> m i i a
  ifail :: String -> m i j a
  
  m `ithen` k = m `ibind` \_ -> k
  {-# INLINE ithen #-}
  
  ifail = error