{-# LANGUAGE FunctionalDependences #-}
module Control.Monad.Indexed.Unsafe
       ( MonadUnsafe (..)
       ) where

import qualified Control.Monad.Indexed as Indexed

class (Monad n, Indexed.Monad m) => MonadUnsafe n m | m -> n where
  unsafe :: n a -> m p q a