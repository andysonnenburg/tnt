{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Indexed.Syntax
       ( (>>=)
       , (>>)
       , return
       , fail
       ) where

import Control.Monad.Indexed.Class

import Prelude (String)

infixl 1 >>, >>=

(>>=) :: Monad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = thenM

(>>) :: Monad m => m i j a -> m j k b -> m i k b
(>>) = thenM_

return :: Monad m => a -> m i i a
return = returnM

fail :: Monad m => String -> m i j a
fail = failM