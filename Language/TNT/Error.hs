{-# LANGUAGE
    DeriveFunctor
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module Language.TNT.Error
       ( ErrorT (..)
       ) where

import Control.Monad.Error.Class

newtype ErrorT e m a = ErrorT
                       { runErrorT :: m (Either e a)
                       } deriving Functor

instance Monad m => Monad (ErrorT e m) where
  return = ErrorT . return . return
  ErrorT m >>= k = ErrorT $ do
    a <- m
    case a of
      Left l -> return . Left $ l
      Right r -> runErrorT (k r)

instance Monad m => MonadError e (ErrorT e m) where
  throwError = ErrorT . return . Left
  ErrorT m `catchError` h = ErrorT $ do
    a <- m
    case a of
      Left e -> runErrorT (h e)
      r@(Right _) -> return r