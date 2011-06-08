module Control.Monad.Indexed.Trans (MonadTrans (..)) where

class MonadTrans t where
  lift :: Monad m => m a -> t m i i a