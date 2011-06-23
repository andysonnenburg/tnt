module Control.Monad.Function
       ( thenM
       , thenM_
       , returnM
       , failM
       ) where

infixl 1 `thenM_`, `thenM`

thenM :: Monad m => m a -> (a -> m b) -> m b
thenM = (>>=)

thenM_ :: Monad m => m a -> m b -> m b
thenM_ = (>>)

returnM :: Monad m => a -> m a
returnM = return

failM :: Monad m => String -> m a
failM = fail