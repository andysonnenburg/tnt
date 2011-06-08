module Control.Monad.Version.Class (MonadVersion (..)) where

import Data.Word

class Monad m => MonadVersion m where
  getMinorVersion :: m Word16
  getMajorVersion :: m Word16